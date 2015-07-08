import org.parboiled2._

class ReadableSyntaxGrammar(val input: ParserInput) extends Parser {
  import Grammar.{Terminal, Nonterminal, GrammarAtom}
  import Transform._
  
  
  
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def InputFile: Rule1[Seq[TransformerBlock]] = rule {
    oneOrMore(transformerBlock) ~ commentNL ~ EOI
  }
  
  def transformerBlock: Rule1[TransformerBlock] = rule { 
    (
      t_begin ~ commentNL ~ 
      declarations ~ commentNL ~ 
      zeroOrMore(transformerBlock).separatedBy(commentNL) ~ 
      zeroOrMore(matcherAndTransformer).separatedBy(commentNL) ~ commentNL ~ 
      t_end
    ) ~> (
      (decls: Seq[BeginPart], tbs: Seq[TransformerBlock], mat: Seq[MatcherAndTransformer]) => 
        TransformerBlock(tbs.toList, decls.toList, mat.toList)
    ) 
  }
    
  
  def matcherAndTransformer = rule {
    t_in ~ commentNL ~ 
    ruleMatchers ~  commentNL ~
    t_out ~ commentNL ~ 
    ruleMatchers ~  commentNL ~
    t_pattern ~ t_space ~ typeEquiv ~ commentNL ~ 
    patterns ~> ((rmtsIn, rmtsOut, teq, patns) => MatcherAndTransformer(rmtsIn.toList, rmtsOut.toList, teq, patns.toList))
  } 
    
  def commentNL    = rule { oneOrMore(commentEOL) }
  def commentEOL   = rule { optional(t_optspace ~ "//" ~ (!t_newLine ~ ANY)) ~ t_newLine }
  
  def declarations = rule { oneOrMore(declaration).separatedBy(commentNL) }
  def ruleMatchers = rule { oneOrMore(ruleMatcher).separatedBy(commentNL) }
  def patterns     = rule { oneOrMore(pattern).separatedBy(commentNL) }
  def typeEquiv    = rule { t_nt ~ t_space ~ "=" ~ t_space ~ t_nt ~> ((n1, n2) => TypeEquiv(Nonterminal(Symbol(n1)), Nonterminal(Symbol(n2)))) }
  
  def t_anyspace   = CharPredicate("\n\r\t ")
  def t_newLine    = CharPredicate("\n\r")
  
  def declaration = rule { nameBinding | nameGen | (t_nt ~> (n => new Nonterminal(Symbol(n)) with BeginPart )) }
  def ruleMatcher = rule { t_nt ~ t_space ~ wspStr("-> ") ~ ruleName ~ t_space ~ oneOrMore(rhsAtom).separatedBy(t_space) ~> ((n, rn, rhs) => GrammarRuleMatcher(NonterminalMatcher(n, "", false), rhs.toList))}
  def pattern     = rule { ruleName ~ t_space ~ oneOrMore(t_variable).separatedBy(t_space) ~ t_space ~ t_equal ~ t_space ~ ruleName ~ t_space ~ oneOrMore(t_variable).separatedBy(t_space) ~> ((rn1, lVars1, rn2, lVars2) => {
      val lPatterns = List(lVars1, lVars2) map (l => l map PatternAtomPrototype) map (seq => seq.toList)
      PatternSynonym(TypedPattern(rn1.name, lPatterns(0), rn1.typ.sym), TypedPattern(rn2.name, lPatterns(1), rn2.typ.sym))
    })
  }
  def patternAtoms = rule {
      (t_variable ~> PatternAtomPrototype) | (t_literal  ~> PatternTerminal)
  }
  def ruleName    = rule { (t_nt ~ "_" ~ t_num) ~> ((n, i) => RuleName(Nonterminal(Symbol(n)), i.toInt))}
  def nameBinding = rule { t_nt ~ t_optspace ~ t_equal ~ t_optspace ~ (
      t_nt      ~> ((s: String) => Nonterminal(Symbol(s)))
    | t_literal ~> ((s: String) => Terminal(s))
  ) ~> ((w, t) => NameBinding(NonterminalMatcher(w.toString, "", false), t))}
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
  
  
  case class TransformerBlock(more: List[TransformerBlock], thisDecls: BeginParts, thisOne: List[MatcherAndTransformer]){
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
  
  case class MatcherAndTransformer(in: List[GrammarRuleMatcher], out: List[GrammarRuleMatcher], patternType: TypeEquiv, patterns: PatternSynonyms) {
    def asString(indent: String) = {
      val indent2 = indent + "  "
      indent + "in\n" +
      indent2 + in.mkString("\n" + indent2) + "\n" + 
      indent + "out\n" + 
      indent2 + out.mkString("\n" + indent2) + "\n" +
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
  case class NameBinding(what: NonterminalMatcher, to: GrammarAtom) extends BeginPart { 
    def asString(indent: String) = indent + what + " = " + to
    
  }
  case class NameGen(typeFor: RuleName, fun: String, args: List[String]) extends BeginPart { 
    def asString(indent: String) = typeFor + " = " + fun + " " + args.mkString(" ")
  }
  
  case class RuleName(typ: Nonterminal, name: Int) {
    override def toString = typ + "_" + name
  }
  
  
}