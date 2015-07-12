import sext._
import scala.util.{Failure, Success}
import org.parboiled2._
import Transform._

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
  val rulesLR = List(
    GrammarRule(s, List(s, plus, f), "1"),
    GrammarRule(s, List(f), "2"),
    GrammarRule(f, List(IntegerTerminal), "3")
    )
  
  val g1 = Grammar(c, rules1)
  val gLR = Grammar(s, rulesLR)
  
  //abstract grammar
  val rules2 = List(
    GrammarRule(a, List(a, plus, a), "1"),
    GrammarRule(a, List(a, mul, a), "2"),
    GrammarRule(a, List(IntegerTerminal), "3")
    )
  val g2 = Grammar(a, rules2)
    
 
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("eliminateLeftRecursionNew.tr")
    val tr = source.mkString
    source.close
    val a = new ReadableSyntaxGrammar(tr)
    a.InputFile.run() match {
      case Success(exprAst) => {
        val (gTrans, psns) = applyTransformerFile(gLR)(exprAst)
        println("Result: \n" + exprAst + "\n---------------\n\n")
        println(gTrans)
        println("Patterns:\n------------\n")
        psns foreach println
      }
      case Failure(e: ParseError) => println("Expr is not valid: " + e.format(tr))
      case Failure(e) => println("Unknown error: " + e)
    }
  }
}