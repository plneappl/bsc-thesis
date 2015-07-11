import org.parboiled2._
import Grammar.{Terminal, Nonterminal, GrammarAtom}
import Transform._

class ReadableSyntaxGrammar(val input: ParserInput) extends Parser {
  import ReadableSyntaxGrammar._
  
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def InputFile: Rule1[TransformerFile] = rule {
    startNonterminal ~ commentNL ~ 
    oneOrMore(transformerBlock) ~ commentNL ~ EOI ~> 
    ((s: StartNonterminal, tbs: Seq[TransformerBlock]) => TransformerFile(s, tbs.toList))
  }
  
  def transformerBlock: Rule1[TransformerBlock] = rule { 
    (
      t_begin ~ commentNL ~ 
      declarations ~ commentNL ~ 
      zeroOrMore(transformerBlock).separatedBy(commentNL) ~ 
      zeroOrMore(matcherAndTransformer).separatedBy(commentNL) ~ commentNL ~ 
      t_end
    ) ~> (
      (decls: Seq[BeginPart], tbs: Seq[TransformerBlock], mat: Seq[TransformerAndPatterns]) => 
        TransformerBlock(tbs.toList, decls.toList, mat.toList)
    ) 
  }
    
  
  def matcherAndTransformer = rule {
    t_in ~ commentNL ~ 
    ruleMatchers ~  commentNL ~
    t_out ~ commentNL ~ 
    ruleMatchers ~  commentNL ~
    t_pattern ~ t_space ~ typeEquiv ~ commentNL ~ 
    patterns ~> ((rmtsIn, rmtsOut, teq, patns) => {
      TransformerAndPatterns(TransformerRule(rmtsIn.toList, rmtsOut.toList), teq, patns.toList)
    })
  } 
    
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace ~ "//" ~ (!t_newLine ~ ANY)) ~ t_newLine ~ t_optspace }
  
  def declarations = rule { 
    zeroOrMore(declaration).separatedBy(commentNL) 
  } 
    
  def ruleMatchers = rule { oneOrMore(ruleMatcher).separatedBy(commentNL) }
  def patterns     = rule { oneOrMore(pattern).separatedBy(commentNL) }
  def startNonterminal = rule { "start" ~ t_space ~ t_nt ~> (x => StartNonterminal(Nonterminal(x))) }
  def typeEquiv    = rule { t_nt ~ t_space ~ "=" ~ t_space ~ t_nt ~> ((n1, n2) => TypeEquiv(Nonterminal(Symbol(n1)), Nonterminal(Symbol(n2)))) }
  
  def t_anyspace   = CharPredicate("\n\r\t ")
  def t_newLine    = CharPredicate("\n\r")
  
  def declaration = rule { nameBinding | nameGen | (t_nt ~> (n => new Nonterminal(Symbol(n)) with BeginPart )) }
  def ruleMatcher = rule { t_nt ~ t_space ~ wspStr("-> ") ~ ruleName ~ t_space ~ oneOrMore(rhsAtom).separatedBy(t_space) ~> (
    (n, rn, rhs) => 
      if(rn.typ.sym.name == n){
        GrammarRuleMatcher(NonterminalMatcher(n, "", false), rhs.toList, rn.name)
      }
      else {
        throw new Exception("Rule name " + rn + " didn't match the Rule's type (" + n + ")!")
      }
    ) 
  }
  
  def pattern     = rule { ruleName ~ t_space ~ oneOrMore(t_variable).separatedBy(t_space) ~ t_space ~ t_equal ~ t_space ~ ruleName ~ t_space ~ oneOrMore(t_variable).separatedBy(t_space) ~> ((rn1, lVars1, rn2, lVars2) => {
      val lPatterns = List(lVars1, lVars2) map (l => l map PatternAtomPrototype) map (seq => seq.toList)
      PatternSynonym(TypedPattern(rn1.name, lPatterns(0), rn1.typ), TypedPattern(rn2.name, lPatterns(1), rn2.typ))
    })
  }
  def patternAtoms = rule {
      (t_variable | t_literal) ~> PatternAtomPrototype
  }
  def ruleName    = rule { (t_nt ~ "_" ~ t_num) ~> ((n, i) => RuleName(Nonterminal(Symbol(n)), i))}
  def nameBinding = rule { (
      t_nt   ~> (x => NonterminalMatcher(x, "", false))
    | t_term ~> (x => TerminalMatcher(x, ""))
  ) ~ t_optspace ~ t_equal ~ t_optspace ~ (
      t_nt      ~> ((s: String) => Nonterminal(s))
    | t_literal ~> ((s: String) => Terminal(s))
  ) ~> ((w, t) => NameBinding(w, t))}
  def nameGen     = rule { ruleName ~ t_equal  ~ capture(oneOrMore(CharPredicate.AlphaNum)) ~ capture(oneOrMore(t_space ~ CharPredicate.AlphaNum)) ~> ((rn, fun, args) => NameGen(rn, fun, args.split(" \t").toList))}
  def rhsAtom     = rule { 
      recursive | nt | ((      
          (t_term             ~> ((s: String) => TerminalMatcher(s, "")))
        | (t_literal          ~> ((s: String) => LiteralMatcher(s, "")))
        | (str(t_int)         ~ push(IntegerMatcher("")))
        | (capture(t_pipe)    ~> ((s: String) => LiteralMatcher(s, "")))
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
  def t_nt               = rule { capture(CharPredicate.UpperAlpha ~ zeroOrMore(CharPredicate.Alpha)) }
  def t_literal          = rule { "\"" ~ !("\"" | t_newLine) ~ capture(zeroOrMore(ANY)) ~ "\"" }
  def t_literalUppercase = rule { capture(oneOrMore(CharPredicate.UpperAlpha)) }
  def t_term             = rule { capture(CharPredicate.LowerAlpha ~ zeroOrMore(CharPredicate.AlphaNum)) }
  def t_variable         = rule { capture(oneOrMore(CharPredicate.LowerAlpha)) }
  def t_num              = rule { capture(oneOrMore(CharPredicate.Digit)) }
  def t_int              = """<int>"""
  def t_newLines         = rule {  oneOrMore(CharPredicate("\n\r")) }
  def t_optNewLines      = rule { zeroOrMore(CharPredicate("\n\r")) }
  def t_lbrace           = """("""                            
  def t_rbrace           = """)"""                            
  def t_recursive        = """r"""                            
  def t_equal            = """="""                            
  def t_pattern          = """pattern"""                      
  def t_underscore       = """_"""                            
  def t_space            = rule {  oneOrMore(CharPredicate(" \t")) }
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }

}

object ReadableSyntaxGrammar{
  case class TransformerFile(s: StartNonterminal, tbs: List[TransformerBlock]){
    override def toString = s + "\n" + 
    tbs.mkString("\n")
  }
  
  case class TransformerBlock(more: List[TransformerBlock], thisDecls: BeginParts, thisOne: List[TransformerAndPatterns]){
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
  
  case class TransformerAndPatterns(transformer: TransformerRule, patternType: TypeEquiv, patterns: PatternSynonyms) {
    def asString(indent: String) = {
      val indent2 = indent + "  "
      transformer.asString(indent2) + "\n" +
      indent + "pattern " + patternType.toString + "\n" + 
      indent2 + patterns.mkString("\n" + indent2)
    }
    
    override def toString = asString("")
  }
  case class TypeEquiv(a: Nonterminal, b: Nonterminal) {
    override def toString = a + " = " + b
  }

  sealed trait BeginPart{ def asString(indent: String): String; override def toString = asString("") }
  type BeginParts = List[BeginPart]
  case class NameBinding(what: TransformerAtom, to: GrammarAtom) extends BeginPart { 
    def asString(indent: String) = indent + what + " = " + to
  }
  
  case class StartNonterminal(s: Nonterminal) {
    override def toString = "Start: " + s
  }
  
  case class NameGen(typeFor: RuleName, fun: String, args: List[String]) extends BeginPart { 
    def asString(indent: String) = typeFor + " = " + fun + " " + args.mkString(" ")
  }
  
  case class RuleName(typ: Nonterminal, name: String) {
    override def toString = typ + "_" + name
  }
  
  
}