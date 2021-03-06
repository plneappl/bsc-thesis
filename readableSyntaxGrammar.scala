import org.parboiled2._
import shapeless._
import Grammar.{Terminal, Nonterminal, GrammarAtom}
import Transform._
import sext._

class ReadableSyntaxGrammar(val input: ParserInput) extends Parser {
  import ReadableSyntaxGrammar._
  
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def InputFile: Rule1[TransformerFile] = rule {
    startNonterminal ~ commentNL ~ 
    oneOrMore(transformerBlock) ~ optional(commentNL) ~ EOI ~> 
    ((s: StartNonterminal, tbs: Seq[TransformerBlock]) => TransformerFile(s, tbs.toList))
  }
  
  def transformerBlock: Rule1[TransformerBlock] = rule { 
    (
      t_begin ~ commentNL ~ 
      zeroOrMore(declaration) ~  
      zeroOrMore(transformerBlock) ~ 
      zeroOrMore(matcherAndTransformer) ~ 
      t_end ~ commentNL
    ) ~> (
      (decls: Seq[TransformerDeclaration], tbs: Seq[TransformerBlock], mat: Seq[TransformerAndPatterns]) => 
        TransformerBlock(tbs.toList, decls.toList, mat.toList)
    ) 
  }
    
  def inOut: RuleN[Seq[GrammarRuleMatcher] :: Seq[GrammarRuleMatcher] :: Boolean :: HNil] = rule {
    t_in ~ commentNL ~ ruleMatchers ~ ((t_out ~ commentNL ~ ruleMatchers ~ push(false))| (t_seq ~ commentNL ~ ruleMatchers ~ push(true)))
  }
  def matcherAndTransformer: Rule1[TransformerAndPatterns] = rule {
    inOut ~ patterns ~> (
      (rmtsIn: Seq[GrammarRuleMatcher], rmtsOut: Seq[GrammarRuleMatcher], nr: Boolean, patnsAutoForce: (Boolean, Boolean, Seq[PatternSynonym])) => {
        val (rmtsIn2, rmtsOut2) = if(nr) numberRules(rmtsIn, rmtsOut) else (rmtsIn, rmtsOut)
        val (auto, force, patns) = patnsAutoForce
        TransformerAndPatterns(TransformerRule(rmtsIn2.toList, rmtsOut2.toList), patns.toList, auto, force)
      }
    )
  } 
    
  def numberRules(rmtsIn: Seq[GrammarRuleMatcher], rmtsOut: Seq[GrammarRuleMatcher]): (Seq[GrammarRuleMatcher], Seq[GrammarRuleMatcher]) = {
    val used = (rmtsIn.map(_.getIds) ++ rmtsOut.map(_.getIds)).flatten.toSet
    //println(rmtsIn)
    //println(rmtsOut)
    //println(used)
    var used1 = used
    val rmtsIn2 = rmtsIn.map(rule => {
      val (r, u) = numberRule(rule, used1)
      used1 = u
      r
    })
    used1 = used
    val rmtsOut2 = rmtsOut.map(rule => {
      val (r, u) = numberRule(rule, used1)
      used1 = u
      r
    })
    //println(rmtsIn2 + "\n" + rmtsOut2)
    (rmtsIn2, rmtsOut2)
  }
    
  def numberRule(r: GrammarRuleMatcher, used: Set[String]): (GrammarRuleMatcher, Set[String])  = {
    var next = 1
    var used1 = used
    val newRhs = r.rhs.map(ma => {
      if(ma.tag != "") ma
      else{
        while(used1(next.toString)) next = next + 1
        used1 = used1 + next.toString
        ma.copy(next.toString)
      }
    })
    (GrammarRuleMatcher(r.lhs, newRhs, r.tag, r.restMatcher), used1)
  }
    
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace) ~ optional("//" ~ zeroOrMore(!t_newLine ~ ANY)) ~ oneOrMore(t_newLine) ~ t_optspace }
      
  def ruleMatchers = rule { oneOrMore(ruleMatcher).separatedBy(commentNL) ~ commentNL ~> (l => l.flatten) }
  def patternKeyword: RuleN[Boolean :: Boolean :: HNil] = rule{t_pattern ~ ((t_space ~ t_autoPattern ~ push(true))  | (push(false) )) ~ ((t_space ~ t_forcePattern ~ push(true))  | (push(false) )) ~ commentNL }
  def patternBlock: Rule1[(Boolean, Boolean, Seq[PatternSynonym])] = rule { 
    patternKeyword ~ zeroOrMore(pattern) ~> ((x1: Boolean, x2: Boolean, y: Seq[PatternSynonym]) => (x1, x2, y)) 
  }
  def patterns: Rule1[(Boolean, Boolean, Seq[PatternSynonym])]    = rule { optional( patternBlock ) ~> 
    ((x: Option[(Boolean, Boolean, Seq[PatternSynonym])]) => {
      x match {
        case Some(l) => l; case None => (false, false, Seq())
      } 
    })
  }
  def startNonterminal = rule { "start" ~ t_space ~ t_nt ~> (x => StartNonterminal(Nonterminal(x))) }
  def typeEquiv    = rule { t_nt ~ t_space ~ "=" ~ t_space ~ t_nt ~> ((n1, n2) => TypeEquiv(Nonterminal(Symbol(n1)), Nonterminal(Symbol(n2)))) }
  
  def t_anyspace   = CharPredicate("\n\r\t ")
  def t_newLine    = CharPredicate("\r\n")
  
  def declaration = rule { (
      nameBinding 
    | nameGen 
    | (
        t_nt ~> (n => NTMatcherDeclaration(NonterminalMatcher(n, ""))) ~ 
        optional("_" ~ t_alphaNum ~> ((x: NTMatcherDeclaration, i) => NTMatcherDeclaration(x.s.copy(i))))
      )
    ) ~ commentNL }
  def ruleMatcher = rule { 
    t_nt ~ t_space ~ wspStr("-> ") ~ ruleMatcherRHSes ~> (
    (n, rhses) => 
      rhses.map(rhs1 => {
        val rn  = rhs1.rn
        val rhs = rhs1.rhsAtoms.toList
        val restMatcher = rhs1.restMatcher
        if(rn.typ.sym.name == n){
          GrammarRuleMatcher(NonterminalMatcher(n, "", false), rhs, rn.name, restMatcher)
        }
        else {
          throw new Exception("Rule name " + rn + " didn't match the Rule's type (" + n + ")!")
        }})
    ) 
  }
  
  def ruleMatcherRHSes = rule {
    oneOrMore(ruleMatcherRHS).separatedBy((commentNL | t_optspace) ~ wspStr("|"))
  }
  
  case class RuleMatcherRHS(rn: RuleName, rhsAtoms: Seq[TransformerAtom], restMatcher: Option[RestMatcher])
  def ruleMatcherRHS = rule {
    ruleName ~ t_space ~ (
      (oneOrMore(rhsAtom).separatedBy(t_space) ~ optional(t_space ~ "..." ~ (t_nt | t_term | push("")) ~> RestMatcher))
    | (push(Seq[TransformerAtom]()) ~ "..." ~ (t_nt | t_term | push("")) ~> (n => Some(RestMatcher(n))))
    ) ~> RuleMatcherRHS
  }
  
  def pattern = rule { 
      typedPattern ~ t_space ~
      t_equal ~ t_space ~ typedPattern ~ commentNL ~> ((p1, p2) => PatternSynonym(p1, p2))
  }
  
  def typedPattern = rule {
    (typedPatternVar ~> (tpv => {ExtractorPattern(tpv.id, tpv.typ)})) |
    (ruleName ~ t_space ~ patternAtoms ~> ((rn, patms) => TypedPattern(rn.name, patms.toList, rn.typ))) 
  }
  
  def typedPatternVar = rule {
    t_lbrace ~ t_variable ~ t_space ~ t_typeSeparator ~ t_space ~ t_nt ~ t_rbrace ~> ((id, typ) => TypedPatternVariable(id, Nonterminal(typ), false))
  }
  
  def patternAtoms = rule { oneOrMore(patternAtom).separatedBy(t_space) }
  
  def patternAtom: Rule1[PatternAtom] = rule {
      patternVar | ("(" ~ typedPattern ~ ")") | patternLit | typedPatternVar
  }
  
  def patternVar = rule { (t_variable ~> PatternAtomPrototype) }
  def patternLit = rule { (t_literal ~> (t => PatternLiteral("", Terminal(t)))) }
  
  def ruleName    = rule { (t_nt ~ "_" ~ t_alphaNum) ~> ((n, i) => RuleName(Nonterminal(Symbol(n)), i))}
  def nameBinding = rule { 
    (
        t_nt   ~> (x => NonterminalMatcher(x, "", false))
      | t_term ~> (x => TerminalMatcher(x, ""))
    ) ~ t_optspace ~ t_equal ~ t_optspace ~ 
    (
        t_nt      ~> ((s: String) => Nonterminal(s))
      | t_literal ~> ((s: String) => Terminal(s))
    ) ~> ((w, t) => NameBinding(w, t))
  }
  def nameGen     = rule { 
    (ruleName | (t_nt ~> (nt => RuleName(Nonterminal(nt), "")))) ~ t_space ~ t_equal ~ t_space ~ 
    t_alphaNum ~ t_space ~
    oneOrMore(t_visible).separatedBy(t_space) ~ t_optspace ~> 
    ((lhs, fun, args) => NameGen(lhs, fun, args.toList))
  }
  
  def rnToNtm(rn: RuleName) = NonterminalMatcher(rn.typ.toString, rn.name, false)
  
  def rhsAtom     = rule { 
      recursive | ((      
          (t_nt                           ~> (n => NonterminalMatcher(n, "", false)))
        | (t_term                         ~> ((s: String) => TerminalMatcher(s, "")))
        | (t_literal                      ~> ((s: String) => LiteralMatcher(s, "")))
        | (str(t_int)                     ~ push(IntegerMatcher("")))
        | ("_" ~ t_nt)                    ~> (n => AnyMatcher(n, ""))
    ) ~ identifier) }
  def nt          = rule { (t_nt ~> (n => NonterminalMatcher(n, "", false))) ~ identifier}
  def recursive   = rule { "r(" ~  nt ~ ")" ~> ((x: TransformerAtom) => x.copy(true))}
  def identifier  = rule { optional(identifierPart ~> ((atom: TransformerAtom, ident: String) => atom.copy(ident))) }   
  
  def identifierPart = rule{ str(":") ~ (t_num | t_variable) }   
  
  def t_in               = """in"""   
  def t_out              = """out"""  
  def t_seq              = """seq"""  
  def t_begin            = """begin"""
  def t_end              = """end"""  
  def t_rightArrow       = """->"""   
  def t_colon            = """:"""    
  def t_pipe             = """|"""
  def t_typeSeparator    = """::"""
  def t_nt               = rule { capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) }
  def t_literal          = rule { 
    ("\"" ~ capture(optional(zeroOrMore(!CharPredicate("\"\n\r") ~ ANY))) ~ "\"") 
  }
  def t_literalUppercase = rule { capture(oneOrMore(CharPredicate.UpperAlpha)) }
  def t_term             = rule { !reservedLower ~ capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) }
  def t_variable         = rule { capture(oneOrMore(CharPredicate.LowerAlpha)) }
  def t_num              = rule { capture(oneOrMore(CharPredicate.Digit)) }
  def t_alphaNum         = rule { capture(oneOrMore(CharPredicate.AlphaNum)) }
  def t_visible          = rule { capture(oneOrMore(CharPredicate.Visible)) }
  def t_int              = """<int>"""
  def t_newLines         = rule {  oneOrMore(CharPredicate("\n\r")) }
  def t_optNewLines      = rule { zeroOrMore(CharPredicate("\n\r")) }
  def t_lbrace           = """("""                            
  def t_rbrace           = """)"""                            
  def t_recursive        = """r"""                            
  def t_equal            = """="""                            
  def t_pattern          = """pattern"""   
  def t_autoPattern      = """auto"""  
  def t_forcePattern     = """force"""                 
  def t_underscore       = """_"""                            
  def t_space            = rule {  oneOrMore(CharPredicate(" \t")) }
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }
  
  def reservedLower      = rule { t_in | t_out | t_seq | t_begin | t_end | t_pattern | t_autoPattern | t_forcePattern }

}

object ReadableSyntaxGrammar{
  case class TransformerFile(s: StartNonterminal, tbs: List[TransformerBlock]){
    override def toString = s + "\n" + 
    tbs.mkString("\n")
  }
  
  case class TransformerBlock(more: List[TransformerBlock], thisDecls: TransformerDeclarations, thisOne: List[TransformerAndPatterns]){
    def asString(indent: String): String = {
      val indent2 = indent + "  "
      indent + "begin\n" + 
      (if(thisDecls.length > 0) { thisDecls.map(_.asString(indent2)).reduce((s1, s2) => s1 + "\n" + s2) + "\n" } else "") + 
      (if(more.length > 0) { more.map(_.asString(indent2)).reduce((s1, s2) => s1 + "\n" + s2) + "\n" } else "") + 
      (if(thisOne.length > 0) { thisOne.map(_.asString(indent)).reduce((s1, s2) => s1 + "\n" + s2) + "\n" } else "") + 
      indent + "end\n"      
    }
    
    override def toString = asString("")
  }  
  
  case class TransformerAndPatterns(transformer: TransformerRule, patterns: PatternSynonyms, auto: Boolean, force: Boolean) {
    def asString(indent: String) = {
      val indent2 = indent + "  "
      transformer.asString(indent2) + "\n" +
      indent + "pattern" + (if(auto) " auto" else "") + (if(force) " force" else "") + "\n" + 
      indent2 + patterns.mkString("\n" + indent2)
    }
    
    override def toString = asString("")
  }
  case class TypeEquiv(a: Nonterminal, b: Nonterminal) {
    override def toString = a + " = " + b
  }

  sealed trait TransformerDeclaration { def asString(indent: String): String; override def toString = asString("") }
  type TransformerDeclarations = List[TransformerDeclaration]
  case class NameBinding(what: TransformerAtom, to: GrammarAtom) extends TransformerDeclaration { 
    def asString(indent: String) = indent + what + " = " + to
  }
  
  case class StartNonterminal(s: Nonterminal) {
    override def toString = "Start: " + s
  }
  case class NTMatcherDeclaration(s: NonterminalMatcher) extends TransformerDeclaration {
    def asString(indent: String) = indent + s
  }
  
  case class NameGen(lhs: RuleName, fun: String, args: List[String]) extends TransformerDeclaration { 
    def asString(indent: String) = indent + lhs + " = " + fun + " " + args.mkString(" ")
  }
  
  case class RuleName(typ: Nonterminal, name: String) {
    override def toString = typ + "_" + name
    def toNonterminalMatcher = NonterminalMatcher(typ.toString, name)
  }
  
  
}