import sext._
import scala.util.{Failure, Success}
import org.parboiled2._
import Transform._
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
    GrammarRule(f, List(leftBrace, c, rightBrace), "4")
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
    
    
  
  
  def main(args: Array[String]): Unit = {
    //HACKY load JPL on Linux  
    if(!System.getProperty("os.name").startsWith("Windows"))
      unsafeAddDir("/usr/lib/swi-prolog/lib/amd64")
    //end HACK


    val source = scala.io.Source.fromFile("eliminateLeftRecursionNew2.tr")
    val tr = source.mkString
    source.close
    val a = new ReadableSyntaxGrammar(tr)
    a.InputFile.run() match {
      case Success(exprAst) => {
        val (gTrans, psns, defs) = applyTransformerFile(gLR)(exprAst)
        println("Transformer:\n------------\n" + exprAst + "\n---------------\n\n")
        println("Starting Grammar:\n------------\n" + gLR + "\n---------------\n\n")
        println("Transformed Grammar:\n------------")
        println(gTrans)
        println("\nPatterns:\n------------")
        psns foreach println
        println("\nDefinitions:\n---------------")
        defs foreach println
        println
        val pli = new PrologInterface
        
        val st = parseWithGrammar(gTrans)("1+2")
        val st2 = Branch(RuleName(s, "2"), List(LeafInteger(5)))
        defs foreach pli.addDefinition
        //pli.loadPLFile("./relation/leftrec2.pl")
        pli.loadDefinitions
        println("\nInput tree:")
        println("---------------")
        println(st)
        println("\nTransforming...")
        val transformedTrees = pli.transformTree(st2, gLR, gTrans)
        println("\nTransformed:")
        println("---------------")
        transformedTrees foreach println
      }
      case Failure(e: ParseError) => println("Expr is not valid: " + e.format(tr))
      case Failure(e) => println("Unknown error: " + e)
    }
  }
}