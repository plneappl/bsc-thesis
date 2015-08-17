import sext._
import PrologInterface._
import ReadableSyntaxGrammar.RuleName

object Test{
  import Grammar._
  val c = Nonterminal('C)
  val a = Nonterminal('A)
  val s = Nonterminal('S)
  val f = Nonterminal('F)
  
  val leftBrace  = Terminal("[")
  val rightBrace = Terminal("]")
  val plus       = Terminal("+")
  val mul        = Terminal("*")
  
  //concrete grammar
  val rules1 = List(
    GrammarRule(c, List(s, plus, c), "1"),
    GrammarRule(c, List(s), "2"),
    GrammarRule(s, List(f, mul, s), "3"),
    GrammarRule(s, List(f), "4"),
    GrammarRule(f, List(IntegerTerminal), "5"),
    GrammarRule(f, List(leftBrace, c, rightBrace), "6")
    )
  val rules3 = List(
    GrammarRule(s, List(s, plus, f), "1"),
    GrammarRule(s, List(f), "2"),
    GrammarRule(f, List(IntegerTerminal), "3"),
    GrammarRule(f, List(leftBrace, s, rightBrace), "4")
    )
  val rulesLR = List(
    GrammarRule(c, List(c, plus, s), "1"),
    GrammarRule(c, List(s), "2"),
    GrammarRule(s, List(s, mul, f), "3"),
    GrammarRule(s, List(f), "4"),
    GrammarRule(f, List(IntegerTerminal), "5")
    )
  
  val g1 = Grammar(c, rules1)
  val gLR = Grammar(s, rules3)
  
  //abstract grammar
  val rules2 = List(
    GrammarRule(a, List(a, plus, a), "1"),
    GrammarRule(a, List(a, mul, a), "2"),
    GrammarRule(a, List(IntegerTerminal), "3")
    )
  val g2 = Grammar(a, rules2)
    
  
  
  
}