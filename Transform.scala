object Transform {
  
  import Grammar._
  import sext._
  import collection.mutable.{ ListBuffer, HashMap, MultiMap, Set => MSet }
  import ReadableSyntaxGrammar._
  import scala.reflect.runtime.universe._
  import PrologInterface.Definition
  import org.jpl7.{Term, Atom, Variable, Compound}
  
  //case class TransformerBlock(more: List[TransformerBlock], thisDecls: BeginParts, thisOne: List[TransformerAndPatterns])
  //case class case class TransformerAndPatterns(transformer: TransformerRule, patternType: TypeEquiv, patterns: PatternSynonyms)
  //sealed trait BeginPart{ def asString(indent: String) }
  //case class NameBinding(what: TransformerAtom, to: GrammarAtom) extends BeginPart  
  //case class NameGen(typeFor: RuleName, fun: String, args: List[String]) extends BeginPart 
  //case class RuleName(typ: Nonterminal, name: String) 

  
  type RuleNameTable = Map[RuleName, RuleName]
  type NameTable     = Map[String, String]
  type DeclaredVars  = Set[TransformerAtom]

  def applyTransformerFile(to: Grammar)(file: TransformerFile): (Grammar, PatternSynonyms) = {
    checkDeclaredVars(file)
    val (grules, patterns) = file.tbs.map(applyTransformerBlock(to)).flatten.unzip
    val grules2 = grules.flatten
    (Grammar(file.s.s, grules2), patterns.flatten)
    
  }
  
  def checkDeclaredVars(f: TransformerFile): DeclaredVars = 
    f.tbs.map(checkDeclaredVars(Set(): DeclaredVars, _)).reduce(_ ++ _)
    
  def checkDeclaredVars(decl: DeclaredVars, b: TransformerBlock): DeclaredVars = {
    var decl2 = decl
    b.thisDecls.map {
      case NTMatcherDeclaration(ntm) => decl2 += ntm
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
    case TypedPattern(n, pc, typ) => {
      val decl2 = checkDeclaredVars(decl, NonterminalMatcher(typ.toString, n)) 
      pc.aggregate(decl2)(checkDeclaredVars, _ ++ _)
    }
    case TypedPatternVariable(id, typ, _) => checkDeclaredVars(decl, NonterminalMatcher(typ.toString, id))
    case _ => {decl}
  }
  
  
  
  def applyTransformerBlock(to: Grammar, prev: SymbolTable = Map())(block: TransformerBlock): List[(GrammarRules, PatternSynonyms)] = {
    var st: SymbolTable = prev
    
    //beginParts:
    block.thisDecls.map {
      case NameBinding(what, to) => {st += ((what, to))}
      case NameGen(typeFor, fun, args) => {}
      case NTMatcherDeclaration(nt) =>  {}
    }
    val more = block.more.map(applyTransformerBlock(to, st)).flatten
    val thisOne = block.thisOne.map(applyMatcherAndTransformer(to, st, block.thisDecls))
    thisOne ++ more
  }
  
  
  def applyMatcherAndTransformer(to: Grammar, st: SymbolTable, decls: TransformerDeclarations)(tap: TransformerAndPatterns): (GrammarRules, PatternSynonyms) = {
    //apply tap.transformer        
    var outRules = List[GrammarRule]()
    var patternSynonyms = Set[PatternSynonym]()
    
    applyRule(tap.transformer, to, st, decls) match {
      //if we were able to apply the rule, update our SymbolTable
      case Some(listOfResultPairs) => {
        for((matchedRules, symTable, ruleNameTable, producedRules) <- listOfResultPairs){ 
          outRules = producedRules ++ outRules
          var patterns = producePatternSynonyms(tap.transformer, matchedRules, producedRules, symTable)
          patternSynonyms = patternSynonyms ++ patterns
          val (pss, defs) = tap.patterns.map(finalizePattern(_, ruleNameTable, matchedRules, producedRules)).unzip
          patternSynonyms = patternSynonyms ++ pss
          
        }
      }
      //we couldn't apply the rule, so we do nothing
      case None => { }
    }  
        
    (outRules, patternSynonyms.toList)
  }
  
  def getGrammarRuleByName(name: RuleName): GrammarRules => Option[GrammarRule] = _.filter(x => x.tag == name.name && x.lhs == name.typ).headOption
  
  def finalizePattern(p: PatternSynonym, nt: RuleNameTable, mr: GrammarRules, pr: GrammarRules): (PatternSynonym, List[Definition]) = {
    //println(p)
    //println(p.lhs.ruleName)
    //println(mr)
    //println(pr)
    //println(p.rhs.ruleName)
    //println(nt)
    val fromRule = getGrammarRuleByName(nt(p.lhs.ruleName))(mr).get
    val   toRule = getGrammarRuleByName(nt(p.rhs.ruleName))(pr).get
    //println(fromRule)
    //println(toRule)
    //println(p)
    //println("----")
    val lpc = finalizeTypedPattern(p.lhs, nt, fromRule, mr, pr)
    val rpc = finalizeTypedPattern(p.rhs, nt,   toRule, pr, mr)
    val ps = PatternSynonym(lpc, rpc)
    val defs = patternSynonymToDefinitions(ps, nt, mr, pr)
    (ps, defs)
  }
  
  
  def finalizeTypedPattern(side: TypedPattern, nt: RuleNameTable, r: GrammarRule, rMe: GrammarRules, rOther: GrammarRules): TypedPattern = {
     val pc = (r.rhs zip side.patternContent).map(x => {
      val (ga, pa) = (x._1, x._2)
      pa match {
        case PatternAtomPrototype(id) => {
          ga match {
            case n: Nonterminal    => TypedPatternVariable(id, n, false)               
            case t: Terminal        => PatternTerminal(id, t)               
            case r: Regex           => PatternTerminal(id, r)                
            case IntegerTerminal    => PatternTerminal(id, IntegerTerminal) 
          }
        }
        case tp: TypedPattern => getGrammarRuleByName(nt(tp.ruleName))(rMe) match {
          case Some(r1) if(r1.lhs == tp.typ) => finalizeTypedPattern(tp, nt, r1, rMe, rOther)
          case _                             => {
            val r1 = getGrammarRuleByName(nt(tp.ruleName))(rOther).get
            finalizeTypedPattern(tp, nt, r1, rOther, rMe)
          }
        }
        case pt: PatternTerminal => pt
        case tpv: TypedPatternVariable => throw new Exception("How even...") //should not happen
      }
    })
    TypedPattern(r.tag, pc, r.lhs)
  }
  
  //rel(a1(XF, YR), s2(XF)).
  //
  def patternSynonymToDefinitions(ps: PatternSynonym, nt: RuleNameTable, l: GrammarRules, r: GrammarRules): List[Definition] = {
    println("===========")
    println(ps)
    println()
    println(nt)
    println()
    l.foreach(println)
    println()
    r.foreach(println)
    println()
    checkVariableTypes(ps)
    print("rel(")
    val (t1, l1) = typedPatternToTerm(ps.lhs, ps.rhs, nt, l, r)
    print(", ")
    val (t2, l2) = typedPatternToTerm(ps.rhs, ps.lhs, nt, r, l)
    println(")")
    println
    println(t1)
    println
    println(t2)
    println
    println(Definition(new Compound(tpRelName(ps.lhs, ps.rhs), Array[Term](t1, t2)), l1 ++ l2))
    println("===========\n")
    List()
  }
  
  def checkVariableTypes(ps: PatternSynonym) = {
    println(ps.lhs.getTypedVars)
    println(ps.rhs.getTypedVars)
  }
  
  
  def tpRelName(tp1: TypedPattern, tp2: TypedPattern): String = tpRelName(tp1.typ, tp2.typ)
  def tpRelName(t1: Nonterminal, t2: Nonterminal) = "rel" + t1 + "to" + t2
  def typedPatternToTerm(tp: TypedPattern, otp: TypedPattern, nt: RuleNameTable, myR: GrammarRules, r: GrammarRules): (Term, List[Term]) = {
    val myRule = getGrammarRuleByName(tp.ruleName)(myR).get
    
    
    val x = (tp.patternContent zip myRule.rhs).map(x1 => { val (pa, ga) = x1; pa match {
      case TypedPatternVariable(id, typ, _) => {
        (new Variable("V" + id + typ), List())
      }
      case PatternTerminal(id, str) => {
        //(new Atom("T" + str.bare + id), List())
        (new Variable(str.bare), List())
      }
      case tp1@TypedPattern(pn, pc, typ) => {
        getGrammarRuleByName(tp1.ruleName)(myR) match {
          case None => {
            val (t, defs) = typedPatternToTerm(tp1, otp, nt, r, myR)
            (new Variable("R" + tp1.ruleName), (new Compound(tpRelName(ga.asInstanceOf[Nonterminal], tp1.typ), Array[Term](new Variable("R" + tp1.ruleName), t))) :: defs)
          }
          case _ => {
            typedPatternToTerm(tp1, otp, nt, myR, r)
          }
        }
        
      }
      //case x => {println("unmatched: " + x); new Atom("")}
    }})
    val (t, defss) = x.unzip
    print(")")
    
    (new Compound("c" + tp.ruleName, t.toArray), defss.flatten)
  }
  
  sealed abstract class TransformerAtom{
    def tag: String
    def copy(t: String): TransformerAtom
    def copy(f: Boolean): TransformerAtom
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
  case class TerminalMatcher(name: String, tag: String) extends TransformerAtom {
    override def toString = if(tag != "") name + ":" + tag else name
    def copy(t: String) = TerminalMatcher(name, t)
    def copy(f: Boolean) = TerminalMatcher(name, tag)
    override def equals(o: Any) = o match {
      case TerminalMatcher(name2, _) => name == name2
      case _ => false
    }
  }
  case class LiteralMatcher(matches: String, tag: String) extends TransformerAtom {
    override def toString = if(tag != "") s""""$matches":$tag""" else s""""$matches""""               //"//again, for syntax highlighting
    def copy(t: String) = LiteralMatcher(matches, t)
    def copy(f: Boolean) = LiteralMatcher(matches, tag)
    override def equals(o: Any) = o match {
      case LiteralMatcher(m2, _) => matches == m2
      case _ => false
    }
  }
  case class IntegerMatcher(tag: String) extends TransformerAtom {
    override def toString = "<int>:" + tag
    def copy(t: String) = IntegerMatcher(t)
    def copy(f: Boolean) = IntegerMatcher(tag)
  }
  
  type TransformerSequence = List[TransformerAtom] 
  type TransformerRules = List[TransformerRule]
  
  case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence, tag: String) {
    override def toString = lhs + "_" + tag + " ->" + rhs.map(_.toString).fold("")(joinStringsBy(" "))
    def ruleName = RuleName(Nonterminal(lhs.name), tag)
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
  def matches(st: SymbolTable, grammarRule: GrammarRule, matcher: GrammarRuleMatcher): Option[SymbolTable] = if(grammarRule.rhs.size == matcher.rhs.size) try {
    var symbolTable = checkSymbolTable(st, matcher.lhs, grammarRule.lhs)
    for((ga, ma) <- (grammarRule.rhs zip matcher.rhs)) {
      (ga, ma) match {
        case (IntegerTerminal, IntegerMatcher(_)) => {}
        case (x1: Nonterminal, x2: NonterminalMatcher) => symbolTable = checkSymbolTable(symbolTable, x2, x1)
        case (x1: Terminal,    x2: TerminalMatcher)    => symbolTable = checkSymbolTable(symbolTable, x2, x1)
        case (Terminal(str1), LiteralMatcher(str2, _))  if (str1 == str2) => { }
        
        case x => {return None}
    }}
    Some(symbolTable)
  } catch { case e: Throwable => None }
  else None
  
  
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

  type TransformationInformation = (GrammarRules, SymbolTable, RuleNameTable, GrammarRules)

  def getSymbols(from: SymbolTable) = from.values.collect {
    case nt: Nonterminal => nt
  }

  //try to match all 'from' rules, on every success: produce 'to' rules
  def applyRule(transformerRules: TransformerRule, grammar: Grammar, startSymbolTable: SymbolTable = Map(), decls: TransformerDeclarations = List()): Option[List[TransformationInformation]] = {
    var selectN = select[GrammarRule, (List[GrammarRuleMatcher], SymbolTable, RuleNameTable, NameTable, GrammarRules)](transformerRules.from.length)(grammar.rules) _
    var matchedRules = selectN(matchRulesToGrammarRules(transformerRules.from, startSymbolTable) _)
    
    //produce the out rules
    var out = List[(GrammarRules, SymbolTable, RuleNameTable, GrammarRules)]()
    
    var usedSymbols: UsedSymbols = Set[Nonterminal]()
    for((gMatchers, st, rnt, nt, gRules) <- matchedRules){
      var (symbolTable, ruleNameTable) = generateSymbols(st, rnt, decls)
      usedSymbols ++= getSymbols(st)
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
    Some(out)
  }
  
  
  
  def generateSymbols(symbolTable: SymbolTable, ruleNameTable: RuleNameTable, decls: TransformerDeclarations): (SymbolTable, RuleNameTable) = {
    var st = symbolTable
    var rnt = ruleNameTable
    for(decl <- decls) decl match {
      case NameGen(lhs, fun, args) => {
        //println(lhs + " = " + fun + " " + args.mkString(" "))
        //println(typeOf[Transform.type].member(newTermName("collapse")).asMethod.paramss)
        val args2 = lhs :: symbolTable :: rnt :: args
        //println("careful, magic: ")     
        val ru = scala.reflect.runtime.universe
        val m = ru.runtimeMirror(getClass.getClassLoader)
        val im = m.reflect(this)
        val methodSym = ru.typeOf[Transform.type].member(ru.newTermName(fun)).asMethod
        val mm = im.reflectMethod(methodSym)
        val res = mm(args2: _*).asInstanceOf[(SymbolTable, RuleNameTable)]
        st = res._1
        rnt = res._2
        //println("-----------------\n")
      }
      case _ => {}
    }
    (st, rnt)
  }
  
  def collapse(lhs: RuleName, st: SymbolTable, rnt: RuleNameTable, a1: String, a2: String): (SymbolTable, RuleNameTable) = {
    val a2split = a2.split("_")
    val rnTyp = st(NonterminalMatcher(a1, "")).asInstanceOf[Nonterminal]
    val rnVar = RuleName(Nonterminal(a2split(0)), a2split(1))
    val rnName = rnt(rnVar)
    val nrntEntry = ((lhs, RuleName(rnTyp, rnName.name)))
    (st, rnt + nrntEntry)
  }
  
  //make a new rule by applying the TransformerAtoms one by one, steadily updating the SymbolTable in case a new Nonterminal is introduced
  def makeOutRule(st: SymbolTable, rnt: RuleNameTable, nt: NameTable, prod: GrammarRuleMatcher, usedSymbols: UsedSymbols, decls: TransformerDeclarations = List()): (SymbolTable, RuleNameTable, UsedSymbols, GrammarRule) = {
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
            println("[warn] couldn't find " + prod.tag + " in (the lesser) NT: " + nt + ".")
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

      case LiteralMatcher(str, _) => (st, usedSymbols, Terminal(str))
      case IntegerMatcher(_) => (st, usedSymbols, IntegerTerminal)
    }
  }
  
  //insert a new Symbol with the target ID
  def extendSymbolTable(st: SymbolTable, id: NonterminalMatcher, usedSymbols: UsedSymbols): (SymbolTable, UsedSymbols) = {
    
    if(st.isDefinedAt(id)) return (st, usedSymbols)
    
    var next = id.name
    
    while(st.values.toList.contains(Nonterminal(next)) || usedSymbols.contains(Nonterminal(next))){
      next = nextLexicographic(next)
    }
    (st + ((id, Nonterminal(next))), usedSymbols + Nonterminal(next))
  }
  
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
        case Some(listOfResultPairs) => {
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
  
  sealed trait PatternAtom { def id: String; def getIds: List[String]; def getLength: Int; def getTypedVars: List[RuleName] = List() }
  case class PatternAtomPrototype(id: String) extends PatternAtom{
    override def toString = id
    def getIds = List(id)
    def getLength = 1
  }
  case class TypedPatternVariable(id: String, typ: Nonterminal, rec: Boolean) extends PatternAtom {
    override def toString = if(id != "") ("(" + id + " :: " + typ + ")") else ("(" + typ + ")")
    def getIds = List(id)
    def getLength = 1
    override def getTypedVars = List(RuleName(typ, id))
  }
  case class PatternTerminal(id: String, str: GrammarAtom) extends PatternAtom{
    override def toString = "(" + id + " :: " + str + ")"
    def getIds = List(id)
    def getLength = 1
  }

  case class TypedPattern(patternName: String, patternContent: List[PatternAtom], typ: Nonterminal) extends PatternAtom {
    def id = ""
    def getIds = patternContent.map(_.getIds).fold(List())((x, y) => x ++ y)
    def getLength = patternContent.map(_.getLength).fold(0)(_ + _)
    override def toString = "(" + typ + "_" + patternName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ + ")"
    def ruleName = RuleName(typ, patternName)
    override def getTypedVars = patternContent.map(_.getTypedVars).flatten
  }
  //Name as String
  //case class TypedPattern(ruleName: String, patternContent: List[PatternAtom], typ: Nonterminal){
  //  override def toString = ruleName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ
  //}
  
  type SeenTags = MultiMap[String, TransformerSequence]
  def newSeenTags():SeenTags = new HashMap[String, MSet[TransformerSequence]] with MultiMap[String, TransformerSequence]
  def addTags(rule: TransformerSequence)(to: SeenTags): SeenTags = {    
    var res = to
    rule.map(_.tag).filter(_ != "").foreach(x => res.addBinding(x, rule))
    res
  }  
  
  def lists[T1](n: Int)(l: List[T1]) = l.combinations(n).map(_.permutations).flatten
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
  //get the PatternSynonym name by looking up, which grammar rule matches the transformerRule
  
  def producePatternSynonyms(tRule: TransformerRule, matchedRules: GrammarRules, producedRules: GrammarRules, symbolTable: SymbolTable): PatternSynonyms = {
    var res = Set[PatternSynonym]()
    
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
        if(getTags(currentFromPattern) != Nil && getTags(currentFromPattern).filter(_ != "") != Nil &&
           getTags(currentFromPattern) == getTags(currentToPattern) && 
           isNotRecursive(currentFromPattern) && isNotRecursive(currentToPattern)){
          res += PatternSynonym(currentFromPattern, currentToPattern)
        }
        
        currentFromPattern = recursivePattern(currentFromPattern, recursiveInsertionFrom)
          currentToPattern = recursivePattern(  currentToPattern, recursiveInsertionTo  )
        
        if(getTags(currentFromPattern) != Nil && getTags(currentFromPattern).filter(_ != "") != Nil && 
          getTags(currentFromPattern) == getTags(currentToPattern)){
          res += PatternSynonym(currentFromPattern, currentToPattern)
        }
        


    }
    numberCountingVariables(res.toList)
    //res.toList
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
        stillFree = stillFree.tail
        TypedPatternVariable(stillFree.head.toString, str, rec)
      }
      case PatternTerminal(id, str) if(id == cv) => {
        stillFree = stillFree.tail
        PatternTerminal(stillFree.head.toString, str)
      }
      case t: TypedPattern => {
        val (tp, sf) = numberCountingVariables(t, stillFree, cv)
        stillFree = sf
        tp
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
  
  def translateRule(matchedRule: GrammarRule, grammarRuleMatcher:  GrammarRuleMatcher, symbolTable: SymbolTable): TypedPattern = {
    TypedPattern(matchedRule.tag, grammarRuleMatcher.rhs.map(translatePatternAtom(symbolTable)), symbolTable(grammarRuleMatcher.lhs).asInstanceOf[Nonterminal])
  }
  
  def translatePatternAtom(symbolTable: SymbolTable)(x: TransformerAtom): PatternAtom =  x match {
    case id@NonterminalMatcher(_, tag, rec) => TypedPatternVariable(tag, symbolTable(id).asInstanceOf[Nonterminal], rec)
    case LiteralMatcher(m, tag) => PatternTerminal(tag, Terminal(m))
    case id@TerminalMatcher(_, tag) => PatternTerminal(tag, symbolTable(id))
    case IntegerMatcher(tag) => PatternTerminal(tag, Terminal("<int>"))
  }
  
  
  
  
}