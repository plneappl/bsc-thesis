object Transform {
  
  import Grammar._
  import sext._
  import collection.mutable.{ ListBuffer, HashMap, MultiMap, Set => MSet }
  import ReadableSyntaxGrammar._
  import scala.reflect.runtime.universe._
  import PrologInterface.Definition
  import org.jpl7.{Term, Atom, Variable, Compound}
  
  //case class TransformerBlock(more: List[TransformerBlock], thisDecls: BeginParts, thisOne: List[TransformerAndPatterns])
  //case class TransformerAndPatterns(transformer: TransformerRule, patterns: PatternSynonyms, auto: Boolean)
  //sealed trait BeginPart{ def asString(indent: String) }
  //case class NameBinding(what: TransformerAtom, to: GrammarAtom) extends BeginPart  
  //case class NameGen(typeFor: RuleName, fun: String, args: List[String]) extends BeginPart 
  //case class RuleName(typ: Nonterminal, name: String) 

  
  type RuleNameTable = Map[RuleName, RuleName]
  type NameTable     = Map[String, String]
  type DeclaredVars  = Set[TransformerAtom]

  
  //this takes a transformerFile, checks, which vars are declared and warns the user about all the other ones.
  //returns all declared vars.
  def checkDeclaredVars(f: TransformerFile): DeclaredVars = 
    f.tbs.map(checkDeclaredVars(Set(): DeclaredVars, _)).reduce(_ ++ _)
    
  def checkDeclaredVars(decl: DeclaredVars, b: TransformerBlock): DeclaredVars = {
    var decl2 = decl
    b.thisDecls.map {
      case NTMatcherDeclaration(ntm) => decl2 += (ntm)
      case NameGen(lhs, _, _) => decl2 += lhs.toNonterminalMatcher
      case NameBinding(ntm: NonterminalMatcher, _) => decl2 += ntm
      case _ => {}
    }
    decl2 = b.thisOne.aggregate(decl2)(checkDeclaredVars, _ ++ _)
    b.more.aggregate(decl2)(checkDeclaredVars, _ ++ _)
    
  }
  def checkDeclaredVars(decl: DeclaredVars, tap: TransformerAndPatterns): DeclaredVars = {
    val decl2 = checkDeclaredVars(decl, tap.transformer)
    tap.patterns.aggregate(decl2)(checkDeclaredVars, _ ++ _)
  }
  def checkDeclaredVars(decl: DeclaredVars, tr: TransformerRule): DeclaredVars = {
    val decl2 = tr.from.aggregate(decl)(checkDeclaredVars, _ ++ _)
    tr.to.aggregate(decl2)(checkDeclaredVars, _ ++ _)
  }
  def checkDeclaredVars(decl: DeclaredVars, grm: GrammarRuleMatcher): DeclaredVars = {
    val decl2 = checkDeclaredVars(decl, grm.lhs)
    grm.rhs.aggregate(decl2)(checkDeclaredVars, _ ++ _)
  }
  def checkDeclaredVars(decl: DeclaredVars, a: TransformerAtom): DeclaredVars = {
    a match {
      case (_: NonterminalMatcher | _: TerminalMatcher) if(!decl(a)) => { 
        println("[warn] You didn't declare " + a + ". I'll ignore this.") 
        decl + a
      }
      case _ => decl
    }
  }
  def checkDeclaredVars(decl: DeclaredVars, p: PatternSynonym): DeclaredVars = {
    val decl2 = checkDeclaredVars(decl, p.lhs) 
    checkDeclaredVars(decl2, p.rhs)
  }
  def checkDeclaredVars(decl: DeclaredVars, p: PatternAtom): DeclaredVars = p match {
    case ExtractorPattern(id, typ) => {
      checkDeclaredVars(decl, NonterminalMatcher(typ.toString, ""))
    }
    case TypedPattern(n, pc, typ) => {
      val decl2 = checkDeclaredVars(decl, NonterminalMatcher(typ.toString, n)) 
      pc.aggregate(decl2)(checkDeclaredVars, _ ++ _)
    }
    case TypedPatternVariable(id, typ, _) => checkDeclaredVars(decl, NonterminalMatcher(typ.toString, id))
    case _ => {decl}
  }
  
  
  //apply a parsed transformer file by recursively descending into it. Check it first though.
  //returns the transformed grammar, resulting pattern synonyms and the corresponding prolog definitions.
  def applyTransformerFile(to: Grammar)(file: TransformerFile): (Grammar, PatternSynonyms, List[Definition]) = {
    checkDeclaredVars(file)
    val (grules, patterns, defs) = file.tbs.map(applyTransformerBlock(to)).map(_._2).flatten.unzip3
    val grules2 = grules.flatten
    (Grammar(file.s.s, grules2.toSet.toList.sortBy(((r: GrammarRule) => r.tag)).sortBy((r: GrammarRule) => r.rhs.length).reverse), patterns.flatten, defs.flatten)
    
  }
  def applyTransformerBlock(to: Grammar, prev: TransformerState = (Set(), Map(), Map()))(block: TransformerBlock): (UsedSymbols, List[(GrammarRules, PatternSynonyms, List[Definition])]) = {
    
    //var (us, st, rnt) = generateSymbols(prev, block.thisDecls)
    var (us, st, rnt) = prev
    
    //beginParts:
    //extract all NonterminalMatchers
    val loopNTDecls = block.thisDecls.map {
      case NameBinding(what, to) => {st += ((what, to)); None }
      case NameGen(typeFor, fun, args) => { None }
      case NTMatcherDeclaration(ntm) =>  { Some(ntm) }
    }.flatten

    val nonterminals: List[Nonterminal] = (to.rules.map(_.lhs) ++ to.rules.map(_.rhs.filter(x => x.isInstanceOf[Nonterminal]).map(x => x.asInstanceOf[Nonterminal])).flatten).toSet.toList

    //todo: for(ntm <- loopntdecls; nt <- nonterminals) {st2 = st + (ntm, nt); more( ..., st2, ...) ... }
    val retList = lists(loopNTDecls.length)(nonterminals).map(nts => {
      val more = block.more.map(b => {
        val st2 = st ++ (loopNTDecls zip nts)
        val (us2, x) = applyTransformerBlock(to, (us, st2, rnt))(b)
        us = us2 
        x
      }).flatten
      val thisOne = block.thisOne.map(b => {
        val (us2, x1, x2, x3) = applyMatcherAndTransformer(to, (us, st, rnt), block.thisDecls)(b)
        us = us2
        (x1, x2, x3)
      })
      more ++ thisOne
    }).flatten.toList
    (us, retList)
  }
  def applyMatcherAndTransformer(to: Grammar, state: TransformerState, decls: TransformerDeclarations)(tap: TransformerAndPatterns): (UsedSymbols, GrammarRules, PatternSynonyms, List[Definition]) = {
    var outRules = List[GrammarRule]()
    var us: UsedSymbols = state._1
    var patternSynonyms = Set[PatternSynonym]()
    var outDefs = List[Definition]()
    
    //apply tap.transformer        
    applyRule(tap.transformer, to, (us, state._2, state._3), decls) match {
      //if we were able to apply the rule produce patterns and prolog definitions
      case Some((us2, listOfResultPairs)) => {
        us ++= us2
        for((matchedRules, symTable, ruleNameTable, producedRules) <- listOfResultPairs){ 
          
          outRules = producedRules ++ outRules

          us ++= getSymbols(symTable)
          if(tap.auto) {
            var pss = producePatternSynonyms(tap.transformer, matchedRules, producedRules, symTable)
            var defins = pss.map(patternSynonymToDefinitions(_, ruleNameTable, matchedRules, producedRules, tap.force))
            patternSynonyms = patternSynonyms ++ pss
            outDefs = defins ++ outDefs
          }
          val (pss2, defins2) = tap.patterns.map(finalizePattern(_, ruleNameTable, matchedRules, producedRules, tap.force)).unzip
          patternSynonyms = patternSynonyms ++ pss2
          outDefs = defins2 ++ outDefs 
        }
      }
      //we couldn't apply the rule, so we do nothing
      case None => { }
    }  
    val swaps = patternSynonyms.filter(ps => ps.lhs.typ != ps.rhs.typ).map(ps => {
      Definition(
        new Compound(tpRelName(ps.rhs, ps.lhs), Array[Term](new Variable("Y"), new Variable("X"))),
        List(new Compound(tpRelName(ps.lhs, ps.rhs), Array[Term](new Variable("X"), new Variable("Y"))))
      )
    }).toSet.toList
    //println(swaps)
    (us, outRules, patternSynonyms.toList, outDefs ++ swaps)
  }
  
  //looks for a grammarRule by it's ruleName (for example S_1).
  def getGrammarRuleByName(name: RuleName): GrammarRules => Option[GrammarRule] = _.filter(x => x.tag == name.name && x.lhs == name.typ).headOption
  def getGrammarRuleByName(name: Option[RuleName]): GrammarRules => Option[GrammarRule] = grs => name.flatMap(getGrammarRuleByName(_)(grs))
  
  //replace patternAtomPrototypes with the mapped/matched Nonterminals.
  def finalizePattern(p: PatternSynonym, nt: RuleNameTable, mr: GrammarRules, pr: GrammarRules, force: Boolean): (PatternSynonym, Definition) = {
    val fromRule = getGrammarRuleByName(nt.get(p.lhs.ruleName))(mr)
    val   toRule = getGrammarRuleByName(nt.get(p.rhs.ruleName))(pr)

    //EXTRACTOR_PATTERN SPECIAL CASE!
    //Since ExtractorPattern is a TypedPattern, if we can't get any rule we had an ExtractorPattern.
    //Then we can use an empty rule of the right type.
    val lpc = finalizeTypedPattern(p.lhs, nt, fromRule getOrElse GrammarRule(p.lhs.typ, List(), ""), mr, pr)
    val rpc = finalizeTypedPattern(p.rhs, nt,   toRule getOrElse GrammarRule(p.rhs.typ, List(), ""), pr, mr)
    val ps = PatternSynonym(lpc, rpc)
    
    val defin = patternSynonymToDefinitions(ps, nt, mr, pr, force)
    (ps, defin)
  }
  def finalizeTypedPattern(side: TypedPattern, nt: RuleNameTable, r: GrammarRule, rMe: GrammarRules, rOther: GrammarRules): TypedPattern = {
    //EXTRACTOR_PATTERN SPECIAL CASE!
    if(side.isInstanceOf[ExtractorPattern]) {
      val ep = side.asInstanceOf[ExtractorPattern]
      //translate the ExtractorPattern's type.
      val ep2 = new ExtractorPattern(ep.id, nt.find(kv => kv._1.typ == ep.typ).get._2.typ)
      return ep2
    }
    
    //move over our pattern together with the corresponding grammarRule and put it's Atoms in Prototypes/TypedPatternVariables.
    //recurse into TypedPatterns, leave Terminals.
    //TODO: check that type, check lengths maybe. Safety only.   
    val pc = (r.rhs zip side.patternContent).map(x => {
      val (ga, pa) = (x._1, x._2)
      pa match {
        case PatternAtomPrototype(id) => grammarAtomWithTagToPatternAtom(ga, id)
        //check if the recursive rule is from our side of the transformation. If not, it's on the other side.
        case tp: TypedPattern => getGrammarRuleByName(nt(tp.ruleName))(rMe) match {
          case Some(r1) => finalizeTypedPattern(tp, nt, r1, rMe, rOther)
            
          case _                             => {
            val r1 = getGrammarRuleByName(nt(tp.ruleName))(rOther).get
            finalizeTypedPattern(tp, nt, r1, rOther, rMe)
          }
        }
        case tpv: TypedPatternVariable => {
          TypedPatternVariable(tpv.id, ga.asInstanceOf[Nonterminal])
        }
        case pt => pt
      }
    })
    TypedPattern(r.tag, pc, r.lhs)
  }
  
  //translates patternSynonyms while checking types and dealing with type errors by adding requirements(the second argument of Definition).
  def patternSynonymToDefinitions(ps: PatternSynonym, nt: RuleNameTable, l: GrammarRules, r: GrammarRules, force: Boolean): Definition = {
    //translate both sides. Each side has to know which variables the other side contains. 
    val (t1, l1) = typedPatternToTerm(ps.lhs, ps.rhs.getTypedVars.toSet, nt, l, r, false, force)
    val (t2, l2) = typedPatternToTerm(ps.rhs, ps.lhs.getTypedVars.toSet, nt, r, l, true, force)

    val defin = Definition(new Compound(tpRelName(ps.lhs, ps.rhs), Array[Term](t1, t2)), (l1 ++ l2).toList)
    defin
  }
  
  //unified names for typeRelations
  def tpRelName(tp1: TypedPattern, tp2: TypedPattern): String = tpRelName(tp1.typ, tp2.typ)
  def tpRelName(t1: Nonterminal, t2: Nonterminal, flip: Boolean = false) = 
    if(flip) "rel" + t2 + "to" + t1
    else "rel" + t1 + "to" + t2
    
  
  def typedPatternToTerm(tp: TypedPattern, otherVars: Set[RuleName], nt: RuleNameTable, myR: GrammarRules, r: GrammarRules, flip: Boolean, force: Boolean): (Term, Set[Term]) = {
  
    //EXTRACTOR_PATTERN SPECIAL CASE!
    //extractor patterns are top-level patterns (at least if parsed from a grammar.)
    //TODO: check this.
    //therefore it's something like x :: T and can be translated into relTto...(VxT, ...)
    if(tp.isInstanceOf[ExtractorPattern]){
      val ep = tp.asInstanceOf[ExtractorPattern]
      return (ep.toTerm, Set())
    }
    
    val myRule = getGrammarRuleByName(tp.ruleName)(myR).get
    val termAndDefs = (tp.patternContent zip myRule.rhs).map(x1 => { val (pa, ga) = x1; pa match {
      case tp1@TypedPattern(pn, pc, typ) => {
        getGrammarRuleByName(tp1.ruleName)(myR) match {
          //this is not the constructor of our grammar, therefore we have to relate a new temporary variable with it
          case None => {
            //should the subpattern produce any type errors, it should (NOT $flip) the resulting Compounds' sides, since it's on the other side 
            val (t, defs) = typedPatternToTerm(tp1, otherVars, nt, r, myR, !flip, force)
            var compoundContent = Array[Term](new Variable("R" + tp1.ruleName), t)
            if(flip) compoundContent = compoundContent.reverse
            (new Variable("R" + tp1.ruleName), defs + (new Compound(tpRelName(ga.asInstanceOf[Nonterminal], tp1.typ, flip), compoundContent)))
          }
          //this is our constructor, so we can just recurse.
          case _ => {
            typedPatternToTerm(tp1, otherVars, nt, myR, r, flip, force)
          }
        }       
      }
      //all patternAtomPrototypes should be replaced by now.
      case _: PatternAtomPrototype => throw new Exception("Non-allowed code path") 
      case x: hasToTerm => (x.toTerm, Set())
      case tpv@TypedPatternVariable(id, typ, _) => {
        val myVar = new Variable("V" + id + typ + (if(!flip && force) "_force" else ""))
        (
          myVar, {
            //check if our variable is directly found on the other side. Otherwise we get a typeError (more like a resolution error).
            if(otherVars.contains(tpv.ruleName) && (force == false || flip == true)) Set()
            else if (otherVars.contains(tpv.ruleName) && !flip) {
              var compoundContent =  Array[Term](myVar, new Variable("V" + id + typ))
              Set(new Compound(tpRelName(typ, typ, false), compoundContent))
            }
            //TypeError: introduce a relation between our variable and the other one with the same name
            else {
              val otherVar = otherVars.find(rn => rn.name == id)
              otherVar match {
                case Some(otherVar1) => {
                  var compoundContent = Array[Term](myVar, new Variable("V" + otherVar1.name + otherVar1.typ))
                  if(flip) compoundContent = compoundContent.reverse
                  Set(new Compound(tpRelName(typ, otherVar1.typ, flip), compoundContent))
                }
                //the other side doesn't even resolve our variable name.
                case None => {
                  println("[warn] Singleton variable: " + myVar)
                  println("[warn] This could mean that you forgot to match this variable on the other side. I'll ignore this.")
                  Set()
                }
              }
            }
          }
        )
      }
    }})
    val (t, defs) = termAndDefs.unzip
    
    (new Compound("c" + tp.ruleName, t.toArray), defs.foldLeft(Set[Term]())((s1, s2) => {s1 ++ s2}))
  }
  
  sealed abstract class TransformerAtom{
    def tag: String
    def copy(t: String): TransformerAtom
    def copy(f: Boolean): TransformerAtom
  }

  case class AnyMatcher(name: String, tag: String) extends TransformerAtom {
    def copy(t: String) = AnyMatcher(name, t)
    def copy(f: Boolean) = AnyMatcher(name, tag)
    override def toString = "_" + (if(tag != "") name + ":" + tag else name)
    override def equals(o: Any) = o match {
      case AnyMatcher(name2, _) => name == name2
      case _ => false
    }
  }

  case class NonterminalMatcher(name: String, tag: String, recursive: Boolean = false) extends TransformerAtom {
    override def toString = (if(recursive) "r(" else "") + name +  (if(tag != "") ":" + tag else "") + (if(recursive) ")" else "")
    def copy(t: String) = NonterminalMatcher(name, t, recursive)
    def copy(f: Boolean) = NonterminalMatcher(name, tag, f)
    override def equals(o: Any) = o match {
      case NonterminalMatcher(name2, tag2, _) => name == name2 
      case _ => false
    }
    def defaultNonterminal: Nonterminal = Nonterminal(Symbol(name))
  }
  //matches any StringTerminal
  case class TerminalMatcher(name: String, tag: String) extends TransformerAtom {
    override def toString = if(tag != "") name + ":" + tag else name
    def copy(t: String) = TerminalMatcher(name, t)
    def copy(f: Boolean) = TerminalMatcher(name, tag)
    override def equals(o: Any) = o match {
      case TerminalMatcher(name2, _) => name == name2
      case _ => false
    }
  }
  //matches only >Terminal(matches)<
  case class LiteralMatcher(matches: String, tag: String) extends TransformerAtom {
    override def toString = if(tag != "") s""""$matches":$tag""" else s""""$matches""""               //"//again, for syntax highlighting
    def copy(t: String) = LiteralMatcher(matches, t)
    def copy(f: Boolean) = LiteralMatcher(matches, tag)
    override def equals(o: Any) = o match {
      case LiteralMatcher(m2, _) => matches == m2
      case _ => false
    }
  }
  //matches an IntegerTerminal
  case class IntegerMatcher(tag: String) extends TransformerAtom {
    override def toString = "<int>:" + tag
    def copy(t: String)  = IntegerMatcher(t)
    def copy(f: Boolean) = IntegerMatcher(tag)
  }
  //matches a  FloatTerminal
  case class FloatMatcher(tag: String) extends TransformerAtom {
    override def toString = "<float>:" + tag
    def copy(t: String)  = FloatMatcher(t)
    def copy(f: Boolean) = FloatMatcher(tag)
  }
  case class RestMatcher(tag: String) extends TransformerAtom {
    override def toString = "..." + tag
    def copy(t: String) = RestMatcher(t)
    def copy(f: Boolean) = RestMatcher(tag)
  }
  
  type TransformerSequence = List[TransformerAtom] 
  type TransformerRules    = List[TransformerRule]
  
  case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence, tag: String, restMatcher: Option[RestMatcher]) {
    override def toString = lhs + "_" + tag + " ->" + rhs.map(_.toString).fold("")(joinStringsBy(" ")) + (if(restMatcher.nonEmpty) (" " + restMatcher.get.toString) else "")
    def ruleName = RuleName(Nonterminal(lhs.name), tag)
    def getIds: Set[String] = rhs.map(_.tag).filter(_ != "").toSet
  }
  case class TransformerRule(from: List[GrammarRuleMatcher], to: List[GrammarRuleMatcher]) {
    def asString(indent: String) = indent + "from:" + 
      from.map(_.toString).fold("")(joinStringsBy("\n  " + indent)) +
      "\n" + indent + "to:" +
      to.map(_.toString).fold("")(joinStringsBy("\n  " + indent))
  }
  case class GrammarTransformer(start: NonterminalMatcher, rules: TransformerRules) {
    def asString(indent: String) = indent + "GrammarTransformer:" + "\n" +
      indent + "Start: " + start + "\n" +
      indent + "Rules:"  + rules.map(_.asString(indent + "  ")).fold("")(joinStringsBy("\n"))

    override def toString = asString("") 
  }
  
  
  
  //NonterminalMatchers can identify themselves with any symbol, this stores the identification while transforming
  type SymbolTable = Map[TransformerAtom, GrammarAtom]
  type UsedSymbols = Set[Nonterminal]
  
  //insert unknown symbols and check known for equality, fail if not equal
  def checkSymbolTable(st: SymbolTable, id: TransformerAtom, sym: GrammarAtom): SymbolTable = {
    st.get(id) match {
      case Some(sym2) if(sym2 == sym) => st
      case None => st + ((id, sym))
    }}
    
  //match a grammar rule with a matcher one atom at a time  
  def matches(st: SymbolTable, grammarRule: GrammarRule, matcher: GrammarRuleMatcher): Option[SymbolTable] = try { 
    if(grammarRule.rhs.size > matcher.rhs.size && matcher.restMatcher.isDefined) {
      val st2 = checkSymbolTable(st, matcher.restMatcher.get, GrammarAtomSequence(grammarRule.rhs.drop(matcher.rhs.size)))
      matches(st2, GrammarRule(grammarRule.lhs, grammarRule.rhs.take(matcher.rhs.size), grammarRule.tag), matcher.copy(restMatcher = None))
    }
    else if(grammarRule.rhs.size == matcher.rhs.size && matcher.restMatcher.isEmpty) {
      var symbolTable = checkSymbolTable(st, matcher.lhs, grammarRule.lhs)
      for((ga, ma) <- (grammarRule.rhs zip matcher.rhs)) {
        (ga, ma) match {
          case (IntegerTerminal,  IntegerMatcher(_)) => {}
          case (x1: Nonterminal,  x2: NonterminalMatcher) => symbolTable = checkSymbolTable(symbolTable, x2, x1)
          case (x1: TerminalLike, x2: TerminalMatcher)    => symbolTable = checkSymbolTable(symbolTable, x2, x1)
          case (Terminal(str1),   LiteralMatcher(str2, _))  if (str1 == str2) => { }
          case (x1,               x2: AnyMatcher) => symbolTable = checkSymbolTable(symbolTable, x2, x1)
          case x => {return None}
      }}
      Some(symbolTable)
    } 
    else None
  } 
  catch { case e: Throwable => None }
  
  //find matching grammarRuleMatchers to grammarRules and match them. For each match:
  //produce a ruleNameTable, which tells us which exact grammarRuleMatcher matched which grammarRule,
  //        a   symbolTable, which tells us which for example NonterminalMatcher matched which Nonterminals,
  //        a     nameTable, which tells us which grammarRuleNames (for example the "k" in S_k) get matched by which grammarRuleMatcherNames
  //and the matching matchers and matched rules.
  def matchRulesToGrammarRules(from: List[GrammarRuleMatcher], startSymbolTable: SymbolTable = Map())(l: GrammarRules): Option[(List[GrammarRuleMatcher], SymbolTable, RuleNameTable, NameTable, GrammarRules)] = {
      var symbolTable: SymbolTable = startSymbolTable
      var ruleNameTable: RuleNameTable = Map()
      var nameTable: NameTable = Map()
      var matchedRules = List[GrammarRule]()
      for((gRule, gMatcher) <- (l zip from)){
        matches(symbolTable, gRule, gMatcher) match {
          case Some(nst) => {
            symbolTable = nst
            ruleNameTable += ((gMatcher.ruleName, gRule.ruleName))
            nameTable += ((gMatcher.tag, gRule.tag))
            matchedRules = gRule :: matchedRules
          }
          case None => return None
        }
      }
      Some((from, symbolTable, ruleNameTable, nameTable, matchedRules))
    }

  def getSymbols(from: SymbolTable) = from.values.collect {
    case nt: Nonterminal => nt
  }

  type TransformerState = (UsedSymbols, SymbolTable, RuleNameTable)

  //try to match all 'from' rules, on every success: produce 'to' rules
  def applyRule(
    transformerRules: TransformerRule, 
    grammar: Grammar, 
    state: TransformerState = (Set(), Map(), Map()),
    decls: TransformerDeclarations = List()): Option[(UsedSymbols, List[(GrammarRules, SymbolTable, RuleNameTable, GrammarRules)])] = {
    //try all combinations of rules and grammar rules with the same number of rules.
    var selectN = select[GrammarRule, (List[GrammarRuleMatcher], SymbolTable, RuleNameTable, NameTable, GrammarRules)](transformerRules.from.length)(grammar.rules) _
    var matchedRules = selectN(matchRulesToGrammarRules(transformerRules.from, state._2) _)
    
    //produce the out rules
    var out = List[(GrammarRules, SymbolTable, RuleNameTable, GrammarRules)]()
    
    var usedSymbols: UsedSymbols = getSymbols(state._2).toSet ++ state._1
    for((gMatchers, st, rnt, nt, gRules) <- matchedRules){
      var (us, symbolTable, ruleNameTable) = generateSymbols((usedSymbols, st, rnt), decls)
      usedSymbols ++= getSymbols(st)
      usedSymbols ++= us
      var newRules: GrammarRules = List()
      for(rule <- transformerRules.to){
        val (nst, nrnt, nus, r) = makeOutRule(symbolTable, ruleNameTable, nt, rule, usedSymbols, decls)
        symbolTable ++= nst
        ruleNameTable ++= nrnt
        newRules = r :: newRules
        usedSymbols ++= nus
      }
      
      out = (gRules, symbolTable, ruleNameTable, newRules) :: out
      
      
    }
    //return produced rules and the SymbolTables
    Some((usedSymbols, out))
  }
  
  
  //use a user-provided function to generate some names. "collapse" is given as an example.
  //these functions have to be of type
    //(RuleName, SymbolTable, RuleNameTable, String*) => (SymbolTable, RuleNameTable)
  //String* denotes any number of String arguments.
  //the function should add some entries to these tables (or remove some, why you would do that I don't know).
  def generateSymbols(startState: TransformerState, decls: TransformerDeclarations): (UsedSymbols, SymbolTable, RuleNameTable) = {
    var state = startState
    for(decl <- decls) decl match {
      case NameGen(lhs, fun, args) => try {
        //println(lhs + " = " + fun + " " + args.mkString(" "))
        //println(typeOf[Transform.type].member(newTermName("collapse")).asMethod.paramss)
        val args2 = state :: lhs :: args
        //println("careful, magic: ")     
        val ru = scala.reflect.runtime.universe
        val m = ru.runtimeMirror(getClass.getClassLoader)
        val im = m.reflect(this)
        val methodSym = ru.typeOf[Transform.type].member(ru.newTermName(fun)).asMethod
        val mm = im.reflectMethod(methodSym)
        state = mm(args2: _*).asInstanceOf[TransformerState]
      
        //println("-----------------\n")
      } catch {
        case e: Throwable => {
          println("[warn] your method " + fun + " threw an exception. Please be careful.")
          println("[warn] " + e)
        }
      }
      case _ => {}
    }
    state
  }
  
  var GLOBALCOUNTER = 0
  def newRuleName(state: TransformerState, lhs: RuleName, a1: String): TransformerState = {
    var rnTarget = RuleName(Nonterminal(a1), GLOBALCOUNTER.toString)
    GLOBALCOUNTER += 1
    val rntValues = state._3.values.toList
    while(rntValues.contains(rnTarget)){
      rnTarget = RuleName(Nonterminal(a1), GLOBALCOUNTER.toString)
      GLOBALCOUNTER += 1
    }
    val rnEntry = ((lhs, rnTarget))
    state.copy(_3 = state._3 + rnEntry)
    //(state._1, state._2, state._3 + rnEntry)
  }
  
  def newName(state: TransformerState, lhs: RuleName, a1: String): TransformerState = {
    var ntTarget = Nonterminal(a1 + GLOBALCOUNTER)
    GLOBALCOUNTER += 1
    println("!!!!!")
    val stValues = state._2.values.toList
    while(stValues.contains(ntTarget) || state._1.contains(ntTarget)){
      ntTarget = Nonterminal(nextLexicographic(ntTarget.toString))
    }

    val stEntry = (NonterminalMatcher(lhs.typ.toString, ""), ntTarget)
    val usEntry = ntTarget
    
    (state._1 + usEntry, state._2 + stEntry, state._3)
  }

  def collapse(state: TransformerState, lhs: RuleName, a1: String, a2: String): TransformerState = {
    val a2split = a2.split("_")
    val rnTyp = state._2(NonterminalMatcher(a1, "")).asInstanceOf[Nonterminal]
    val rnVar = RuleName(Nonterminal(a2split(0)), a2split(1))
    val rnName = state._3(rnVar)
    val nrntEntry = ((lhs, RuleName(rnTyp, rnName.name)))
    (state._1, state._2, state._3 + nrntEntry)
  }
  
  //make a new rule by applying the TransformerAtoms one by one, steadily updating the SymbolTable in case a new Nonterminal is introduced
  def makeOutRule(
    st: SymbolTable, 
    rnt: RuleNameTable, 
    nt: NameTable, 
    prod: GrammarRuleMatcher, 
    usedSymbols: UsedSymbols, 
    decls: TransformerDeclarations = List()): 
  (SymbolTable, RuleNameTable, UsedSymbols, GrammarRule) = {
    
    var (st2, us2, lhs) = applyMatcher(st, prod.lhs, usedSymbols)
    val lhs2 = lhs.asInstanceOf[Nonterminal]
    var rhs = List[GrammarAtom]()
    for(atom <- prod.rhs){
      val (st3, us3, at) = applyMatcher(st2, atom, us2)
      //prepend for speed, then reverse at the end
      rhs = at +: rhs
      st2 = st3
      us2 = us3
    }
    rhs = rhs.reverse
    if(prod.restMatcher.nonEmpty){
      rhs = rhs ++ st(prod.restMatcher.get).asInstanceOf[GrammarAtomSequence].seq
    }
    
    rnt.get(prod.ruleName) match {
      case Some(name) => (st2, rnt, us2, GrammarRule(lhs2, rhs, name.name))
      case None       => {
        println("[warn] couldn't find " + prod.ruleName + " in RNT: " + rnt + ".")
        nt.get(prod.tag) match {
          case Some(name) => {
            println("[warn] I guessed " + name + " based on my (lesser) NT.")
            (st2, rnt + ((prod.ruleName, RuleName(lhs2, name))), us2, GrammarRule(lhs2, rhs, name))
          }
          case None       => {
            println("[warn] couldn't find " + prod.tag + " in the (lesser) NT: " + nt + ".")
            println("[warn] I guessed " + prod.ruleName.name + " based on your production rule.")
            (st2, rnt + ((prod.ruleName, RuleName(lhs2, prod.ruleName.name))), us2, GrammarRule(lhs2, rhs, prod.ruleName.name))
          }
        }   
      }
    }
  }
  
  //produce a GrammarAtom from a TransformerAtom by looking up in the SymbolTable and adding new IDs
  def applyMatcher(st: SymbolTable, a: TransformerAtom, usedSymbols: UsedSymbols): (SymbolTable, UsedSymbols, GrammarAtom) = {
     a match {
      case id: NonterminalMatcher => {
        val (st2, us2) = extendSymbolTable(st, id, usedSymbols)
        
        (st2, us2, st2(id))
      }
      case id: TerminalMatcher => (st, usedSymbols, st(id))
      case id: AnyMatcher => (st, usedSymbols, st(id))

      case LiteralMatcher(str, _) => (st, usedSymbols, Terminal(str))
      case IntegerMatcher(_) => (st, usedSymbols, IntegerTerminal)
      case FloatMatcher(_) => (st, usedSymbols, FloatTerminal)
    }
  }
  
  //insert a new Symbol with the target ID
  def extendSymbolTable(st: SymbolTable, id: NonterminalMatcher, usedSymbols: UsedSymbols): (SymbolTable, UsedSymbols) = {
    //plain lookup
    if(st.isDefinedAt(id)) return (st, usedSymbols)
    
    //maybe we know S1 but not S1:3?
    val typeTable = symbolTableToTypeTable(st)
    typeTable.get(id.name) match {
      case Some(typ) => {
        return (st + ((id, typ)), usedSymbols + typ)
      }
      //nope. Let's introduce a new one.
      case None => {}
    }
    
    var next = id.name
    
    while(st.values.toList.contains(Nonterminal(next)) || usedSymbols.contains(Nonterminal(next))){
      next = nextLexicographic(next)
    }
    (st + ((id, Nonterminal(next))), usedSymbols + Nonterminal(next))
  }
  
  //names to types instead of matchers to grammar atoms => subset of the symbolTable
  def symbolTableToTypeTable(st: SymbolTable): Map[String, Nonterminal] = st.toList.filter(_._1.isInstanceOf[NonterminalMatcher]).map(kv => (kv._1.asInstanceOf[NonterminalMatcher].name, kv._2.asInstanceOf[Nonterminal])).toMap ++ st.toList.filter(x => (x._1.isInstanceOf[AnyMatcher] && x._2.isInstanceOf[Nonterminal])).map(kv => (kv._1.asInstanceOf[AnyMatcher].name, kv._2.asInstanceOf[Nonterminal])).toMap
  
  def getNthVariableName(n: Int): String = {
    var cur = "A"
    for(i <- 0 until n){cur = nextLexicographic(cur)}
    cur
  }
  
  //increment on Strings over {\epsilon, A, B, C, ..., Z}
  def nextLexicographic(nts: String): String = {
    // \epsilon+1 == "A"
    if(nts.length == 0) "A"
    else {
      var res = ""
      var incrementing = true
      for(c <- nts.reverseIterator){
        //carry
        if(incrementing && c == 'Z') {
          res = 'A' + res
        }
        else if(incrementing && c == 'z') {
          res = 'z' + res
        }
        else if(incrementing && c == '9') {
          res = '0' + res
        }
        //increment
        else if(incrementing){
          res = ((c + 1).asInstanceOf[Char]) + res
          incrementing = false
        }
        //copy
        else{
          res = c + res
        }
      }
      if(incrementing) res = 'A' + res
      res
    }
  }
  

  //to transform a Grammar, we take every transformRule and apply it, meanwhile updating the SymbolTable
  def transformGrammar(transformer: GrammarTransformer)(grammar: Grammar): (Grammar, PatternSynonyms) = {
    var outRules = List[GrammarRule]()
    var patternSynonyms = List[PatternSynonym]()
    var i = 1
    for(rule <- transformer.rules) {
      applyRule(rule, grammar) match {
        //if we were able to apply the rule, update our SymbolTable
        case Some((_, listOfResultPairs)) => {
          for((matchedRules, symTable, _, producedRules) <- listOfResultPairs){ 
            val (producedRulesTagged, i1) = tagGrammarRules(producedRules, i)
            i = i1
            outRules = producedRulesTagged ++ outRules
            var patterns = producePatternSynonyms(rule, matchedRules, producedRulesTagged, symTable)
            patternSynonyms = patternSynonyms ++ patterns
          }
        }
        //we couldn't apply the rule, so we do nothing
        case None => { }
      }  
    }
    //appending ++ grammar.rules.filter(x => !x.matched) to outRules would be nice, but mixes up nonterminal names
    (Grammar(Nonterminal(Symbol(transformer.start.name)), outRules), patternSynonyms)
  }
  
  def tagGrammarRules(rules: List[GrammarRule], nextTag: Int): (List[GrammarRule], Int) = rules match {
    case (GrammarRule(l, r, _)) :: rules2 => {
      val (l1, i) = tagGrammarRules(rules2, nextTag + 1)
      ((GrammarRule(l, r, nextTag.toString)) :: l1, i)
    }
    case Nil => (List[GrammarRule](), nextTag)
  }
  
  
  
    
  case class PatternSynonym(lhs: TypedPattern, rhs: TypedPattern){
    override def toString = lhs.toString + " = " + rhs.toString
  }
  type PatternSynonyms = List[PatternSynonym]
  
  sealed trait PatternAtom { 
    def id: String 
    def getIds: List[String]
    def getLength: Int
    def getTypedVars: List[RuleName] = List() 
  }
  //standard implementation for most Atoms
  sealed trait PatternAtomStandard extends PatternAtom {
    def id: String
    def str: GrammarAtom
    override def toString = "(" + id + " :: " + str + ")"
    def getIds = List(id)
    def getLength = 1
  }
  //interface for everything that has an easy translation to Terms
  sealed trait hasToTerm extends PatternAtom {
    def toTerm: Term
  }
  case class PatternAtomPrototype(id: String) extends PatternAtom {
    override def toString = id
    def getIds = List(id)
    def getLength = 1
  }
  case class TypedPatternVariable(id: String, typ: Nonterminal, rec: Boolean = false) extends PatternAtom {
    override def toString = if(id != "") ("(" + id + " :: " + typ + ")") else ("(" + typ + ")")
    def getIds = List(id)
    def getLength = 1
    def ruleName = RuleName(typ, id)
    override def getTypedVars = List(ruleName)
  }
  //PatternLiterals are directly translated into PrologStrings.
  case class PatternLiteral(id: String, str: GrammarAtom) extends PatternAtomStandard with hasToTerm {
    def toTerm = new Atom(str.bare)
  }
  //for variable terminals 
  sealed trait PatternTerminalTrait extends PatternAtomStandard with hasToTerm {
    def toTerm = new Variable("V" + id + str)
  }
  
  case class PatternTerminal(id: String, str: GrammarAtom) extends PatternTerminalTrait
  case class PatternInteger(id: String) extends PatternTerminalTrait {
    def str = IntegerTerminal
    override def toTerm = new Variable("V" + id + "int")
  }
  case class PatternFloat(id: String) extends PatternTerminalTrait {
    def str = FloatTerminal
    override def toTerm = new Variable("V" + id + "float")
  }


  case class TypedPattern(patternName: String, patternContent: List[PatternAtom], typ: Nonterminal) extends PatternAtom {
    def id = ""
    def getIds = patternContent.map(_.getIds).fold(List())((x, y) => x ++ y)
    def getLength = patternContent.map(_.getLength).fold(0)(_ + _)
    override def toString = "(" + typ + "_" + patternName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ + ")"
    def ruleName = RuleName(typ, patternName)
    override def getTypedVars = patternContent.map(_.getTypedVars).flatten
  }
  
  class ExtractorPattern(epid: String, typ: Nonterminal) extends TypedPattern("KW_EXTRACTOR", List(), typ) with hasToTerm {
    override def getTypedVars = List(RuleName(typ, id))
    override def toString = "(" + id + " :: " + typ + ")"
    override def id = epid
    def toTerm = new Variable("V" + id + typ)
  }
  object ExtractorPattern{
    def apply(id: String, typ: Nonterminal) = new ExtractorPattern(id, typ)
    def unapply(x: ExtractorPattern) = Some((x.id, x.typ))
  }
  
  type SeenTags = MultiMap[String, TransformerSequence]
  def newSeenTags():SeenTags = new HashMap[String, MSet[TransformerSequence]] with MultiMap[String, TransformerSequence]
  def addTags(rule: TransformerSequence)(to: SeenTags): SeenTags = {    
    var res = to
    rule.map(_.tag).filter(_ != "").foreach(x => res.addBinding(x, rule))
    res
  }  
  
  //all sublists of l in all orders
  def lists[T1](n: Int)(l: List[T1]) = l.combinations(n).map(_.permutations).flatten
  //[Use case] map a function on all sublists of a List l and keep only valid results
  def select[T1, T2](n: Int)(l: List[T1])(f: List[T1] => Option[T2]): List[T2] = lists(n)(l).map(f).flatten.toList

  def pairs(max1: Int, max2: Int) = for(i <- (1 to (max1 + max2)).view; j1 <- (1 to max1).view; if(i - j1 >= 1 && i - j1 <= max2)) yield (j1, i - j1)
  
  def getTags(p: TypedPattern): List[String] = {
    p.getIds.filter(_ != "").sorted
  }
  
  def isNum: String => Boolean = s =>{
    val num = "[0-9]+".r
    s match {
      case num() => true
      case _ => false
    }
  }
  
  //produce all PatternSynonyms where all variables on the right side get resolved by the left side
  //get the PatternSynonym name by looking up which grammar rule matches the transformerRule
  def producePatternSynonyms(tRule: TransformerRule, matchedRules: GrammarRules, producedRules: GrammarRules, symbolTable: SymbolTable): PatternSynonyms = {
    var res = Set[PatternSynonym]()
    //try and match every fromRule with every toRule
    //match if all variable names are resolved
    for((i, j) <- pairs(tRule.from.size, tRule.to.size); 
    		recursiveFrom <- tRule.from;
        recursiveTo <- tRule.to;
        fromRules <- lists(i)(tRule.from);
        toRules   <- lists(j)(tRule.to)
        ) {
      //if(((needed.keySet &~ found.keySet) union (found.keySet &~ needed.keySet)).isEmpty && !found.isEmpty) {
        var fromRulesPatterns      = fromRules.map(x => translateRule(findMatchingGrammarRule(x,  matchedRules, symbolTable), x, symbolTable))
        var   toRulesPatterns      =   toRules.map(x => translateRule(findMatchingGrammarRule(x, producedRules, symbolTable), x, symbolTable))
        var currentFromPattern     = fromRulesPatterns.head
        var currentToPattern       = toRulesPatterns.head  
        var insertInFrom           = fromRulesPatterns.tail
        var insertInTo             = toRulesPatterns.tail
        //switch from/to, since we want to insert from the other side
        val recursiveInsertionTo   = translateRule(findMatchingGrammarRule(recursiveFrom,  matchedRules, symbolTable), recursiveFrom, symbolTable)
        val recursiveInsertionFrom = translateRule(findMatchingGrammarRule(recursiveTo,   producedRules, symbolTable), recursiveTo,   symbolTable)
        for(currentInsert <- insertInFrom){
          currentFromPattern = replaceAll(currentFromPattern, currentInsert)
        }
        for(currentInsert <- insertInTo  ){
            currentToPattern = replaceAll(  currentToPattern, currentInsert)
        }
        /* patterns may not:
         * - have unresolved variables (i.e. tags must be the same on both sides)
         * - have dangling variables (i.e. TypedVariables with no tag, these should all be recursively replaced)
         * - be recursive (unless explicitely trying to get those)
         */
        if(!hasDanglingVariables(currentFromPattern) && !hasDanglingVariables(currentToPattern) &&
            getTags(currentFromPattern) != Nil && getTags(currentFromPattern).filter(_ != "") != Nil &&
            getTags(currentFromPattern) == getTags(currentToPattern) && 
            isNotRecursive(currentFromPattern) && isNotRecursive(currentToPattern)){
          res += PatternSynonym(currentFromPattern, currentToPattern)
        }
        currentFromPattern = recursivePattern(currentFromPattern, recursiveInsertionFrom)
          currentToPattern = recursivePattern(  currentToPattern, recursiveInsertionTo  )
        
        if(!hasDanglingVariables(currentFromPattern) && !hasDanglingVariables(currentToPattern) &&
            getTags(currentFromPattern) != Nil && getTags(currentFromPattern).filter(_ != "") != Nil && 
            getTags(currentFromPattern) == getTags(currentToPattern)){
          res += PatternSynonym(currentFromPattern, currentToPattern)
        }
        


    }
    numberCountingVariables(res.toList)
    //res.toList
  }
    
  def hasDanglingVariables(t: TypedPattern): Boolean = {
    t.patternContent.map{
      case TypedPatternVariable(tag, _, _) if(tag == "") => return true
      case t: TypedPattern if(hasDanglingVariables(t)) => return true
      case _ => {}
    }
    return false
  }
  
  def isRecursive: TypedPattern => Boolean = ! isNotRecursive(_)
  
  def isNotRecursive(what: TypedPattern): Boolean = {
    what.patternContent.foreach { x => x match {
        case TypedPatternVariable(_, _, true) => return false
        case t: TypedPattern if(isNotRecursive(t)) => return false
        case _ => {}
      }
    }
    return true
  }
  
  def recursivePattern(in: TypedPattern, what: TypedPattern): TypedPattern = {
    val newContent = in.patternContent.map(x => x match {
      case TypedPatternVariable(_, _, true) => what
      case t: TypedPattern => recursivePattern(t, what)
      case x => x  
    })
    TypedPattern(in.patternName, newContent, in.typ)
  }
  
   def numberCountingVariables(p: PatternSynonyms): PatternSynonyms = p.map(numberCountingVariables)
  
  def numberCountingVariables(p: PatternSynonym): PatternSynonym = {
    var lhs = p.lhs
    var rhs = p.rhs
    val countingVars = getTags(lhs).toSet[String].filter(!isNum(_))
    var free = (1 to (lhs.getLength + rhs.getLength)).filter(x => !(lhs.getIds.contains(x.toString))).toList
    for(countingVar <- countingVars){
      val (nlhs, nfree) = numberCountingVariables(lhs, free, countingVar)
      val (nrhs, _) = numberCountingVariables(rhs, free, countingVar)
      lhs = nlhs
      rhs = nrhs
      free = nfree
    }
    PatternSynonym(lhs, rhs)
  }
  
  def numberCountingVariables(p: TypedPattern, free: List[Int], cv: String): (TypedPattern, List[Int]) = {
    var stillFree = free
    val newContent = p.patternContent.map(x => x match {
      case TypedPatternVariable(id, str, rec) if(id == cv) => {
        val nID = stillFree.head
        stillFree = stillFree.tail
        TypedPatternVariable(nID.toString, str, rec)
      }
      case PatternTerminal(id, str) if(id == cv) => {
        val nID = stillFree.head
        stillFree = stillFree.tail
        PatternTerminal(nID.toString, str)
      }
      case t: TypedPattern => {
        val (tp, sf) = numberCountingVariables(t, stillFree, cv)
        stillFree = sf
        tp
      }
      case PatternLiteral(id, str) if(id == cv) => {
        val nID = stillFree.head
        stillFree = stillFree.tail
        PatternLiteral(nID.toString, str)
      }
      case x => x
    })
    (TypedPattern(p.patternName, newContent, p.typ), stillFree)
  }
  
  
  def findMatchingGrammarRule(what: GrammarRuleMatcher, in: GrammarRules, symbolTable: SymbolTable): GrammarRule = in.filter(matches(symbolTable, _, what) != None).head
  
  def replaceAll(in: TypedPattern, what: TypedPattern): TypedPattern = {
    val newContent = in.patternContent.map(x => x match {
      case TypedPatternVariable(_, typ, _) if(typ == what.typ) => what
      case t: TypedPattern => replaceAll(t, what)
      case x => x   
    })
    TypedPattern(in.patternName, newContent, in.typ)
  }
  
  def matching(from: GrammarRuleMatcher, to: GrammarRuleMatcher): Boolean = {
    var any = false
    for(toAtom <- to.rhs){
      if(toAtom.tag != "" && !(names(from)(toAtom))) return false
      else if(toAtom.tag != "") any = true
    }
    return any
  }
  
  def names(from: GrammarRuleMatcher)(toAtom: TransformerAtom): Boolean = {
    for(fromAtom <- from.rhs){ 
      if(fromAtom.tag == toAtom.tag) return true
    }
    return false
  }
  
  def translateRule(matchedRule: GrammarRule, grammarRuleMatcher: GrammarRuleMatcher, symbolTable: SymbolTable): TypedPattern = {
    val rhs = grammarRuleMatcher.rhs.map(translatePatternAtom(symbolTable)) ++ (
        if(grammarRuleMatcher.restMatcher.nonEmpty){
          val gas = symbolTable(grammarRuleMatcher.restMatcher.get).asInstanceOf[GrammarAtomSequence].seq
          val tags = (Stream from 1).map("...ID" + _.toString)
          (gas zip tags).map( 
            x => (
              grammarAtomWithTagToPatternAtom(x._1, x._2)
            )
          )
        }
        else List()
      )
    TypedPattern(matchedRule.tag, rhs, symbolTable(grammarRuleMatcher.lhs).asInstanceOf[Nonterminal])
  }
  
  def translatePatternAtom(symbolTable: SymbolTable)(x: TransformerAtom): PatternAtom = x match {
    case id@NonterminalMatcher(_, tag, rec) => TypedPatternVariable(tag, symbolTable(id).asInstanceOf[Nonterminal], rec)
    case LiteralMatcher(m, tag) => PatternLiteral(tag, Terminal(m))
    case id@TerminalMatcher(_, tag) => grammarAtomWithTagToPatternAtom(symbolTable(id), tag)
    case id@AnyMatcher(_, tag) => grammarAtomWithTagToPatternAtom(symbolTable(id), tag)
    case IntegerMatcher(tag) => PatternInteger(tag)
    case FloatMatcher(tag) => PatternFloat(tag)
  }
  
  
  def grammarAtomWithTagToPatternAtom(ga: GrammarAtom, tag: String) = ga match {
    //for most of the stuff we don't know, what's inside it's node, so we use a variable.
    case i@IntegerTerminal => PatternInteger(tag)
    case f@FloatTerminal   => PatternFloat(tag) 
    case r: Regex          => PatternTerminal(tag, r)
    case n: Nonterminal    => TypedPatternVariable(tag, n)
    
    //only stringTerminals only match one thing, so we can really use a terminal.
    case t: Terminal       => PatternLiteral(tag, t)
  }
  
  
}