import sext._
import scala.util.{Failure, Success}
import org.parboiled2._

object Test{ 
  def main(args: Array[String]): Unit = {
    val source = scala.io.Source.fromFile("newDef.tr")
    val tr = source.mkString
    source.close
    val a = new ReadableSyntaxGrammar(tr)
    a.InputFile.run() match {
      case Success(exprAst) => println("Result: \n" + exprAst(0))
      case Failure(e: ParseError) => println("Expr is not valid: " + e.format(tr))
      case Failure(e) => println("Unknown error: " + e)
    }
  }
}