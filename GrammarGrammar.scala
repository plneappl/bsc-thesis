import Grammar.{Parser => _, Grammar => GrammarCC, _}
import org.parboiled2._
import shapeless._
import Transform._
import sext._
import ReadableSyntaxGrammar.RuleName

class GrammarGrammar(val input: ParserInput) extends Parboiled2Parser[GrammarCC] {
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace) ~ optional("//" ~ zeroOrMore(!t_newLine ~ ANY)) ~ oneOrMore(t_newLine) ~ t_optspace }
  
  def ruleName    = rule { (t_nt ~ "_" ~ t_alphaNum) ~> ((n, i) => RuleName(Nonterminal(n), i))}
  
  def InputFile: Rule1[GrammarCC] = rule {
    t_start ~ t_space ~ (t_nt ~> ((s: String) => Nonterminal(s))) ~ commentNL ~
    oneOrMore(grammarRule).separatedBy(commentNL) ~ optional(commentNL) ~> ((s: Nonterminal, rls: Seq[Seq[GrammarRule]]) => GrammarCC(s, rls.flatten.toList)) 
  }
  
  def grammarRule: Rule1[Seq[GrammarRule]] = rule {
    (t_nt ~> (s => ())) ~ t_space ~ t_rightArrow ~ t_space ~ grammarRuleRHS 
  }
  def grammarRuleRHS: Rule1[Seq[GrammarRule]] = rule {
    oneOrMore(
      ruleName ~ t_space ~ oneOrMore(grammarAtom).separatedBy(t_space) ~> ((x, y) => (x, y))
    ).separatedBy(t_space ~ t_pipe ~ t_space) ~> (
      (rhs: Seq[(RuleName, Seq[GrammarAtom])]) => rhs.map(x => GrammarRule(x._1.typ, x._2.toList, x._1.name))
    )
  }
  
  def grammarAtom: Rule1[GrammarAtom] = rule {
    nonterminal | terminal | regex | integer | float
  }
  
  def nonterminal: Rule1[GrammarAtom] = rule { t_nt ~> ((s: String) => Nonterminal(s)) }
  def terminal   : Rule1[GrammarAtom] = rule { t_literal ~> Terminal }
  def regex      : Rule1[GrammarAtom] = rule { "r" ~ t_literal ~> Regex }
  def integer    : Rule1[GrammarAtom] = rule { t_int ~ push(IntegerTerminal) }
  def float      : Rule1[GrammarAtom] = rule { t_float ~ push(FloatTerminal) }
  
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
  def t_float            = """<float>"""
  def t_newLines         = rule {  oneOrMore(CharPredicate("\n\r")) }
  def t_optNewLines      = rule { zeroOrMore(CharPredicate("\n\r")) }
  def t_lbrace           = """("""                            
  def t_rbrace           = """)"""                            
  def t_recursive        = """r"""                            
  def t_equal            = """="""                            
  def t_pattern          = """pattern"""   
  def t_autoPattern      = """auto"""        
  def t_start            = """start"""           
  def t_underscore       = """_"""                            
  def t_space            = rule {  oneOrMore(CharPredicate(" \t")) }
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }
  def t_anyspace   = CharPredicate("\n\r\t ")
  def t_newLine    = CharPredicate("\r\n")
  
  def reservedLower      = rule { t_int | t_float }
}