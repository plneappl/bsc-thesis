object Main {
  import PrologInterface._
  import scala.util.{Failure, Success}
  import Grammar.{parseWithGrammar, SyntaxTree}
  import org.parboiled2._
  
  
  type TransformerFunction = (SyntaxTree => TransformationResult)
  
  def main(args: Array[String]): Unit = {
    if(args.length == 0){
      showUsage
    } 
    else{
      //HACKY load JPL on Linux  
      if(!System.getProperty("os.name").startsWith("Windows"))
        unsafeAddDir("/usr/lib/swi-prolog/lib/amd64")
      //end HACK
      
      
      val ti = parse[TransformInstructions, TransformInstructionsFile](args(0))(s => new TransformInstructions(s))
      val inputGrammar = parse[GrammarGrammar, Grammar.Grammar](ti.grammar)(s => new GrammarGrammar(s))
      val (gTrans, fwt, bwt) = transformGrammarWithFile(inputGrammar, ti.transformer, maxDepth = Some(8))
      ti.commands.foreach{
        case parseWithOriginal(input) => parseTest(inputGrammar, input, fwt, bwt)
        case parseWithTransformed(input) => parseTest(gTrans, input, bwt, fwt)
      }
    }
  }
  
  def parseTest(g: Grammar.Grammar, i: String, fwt: TransformerFunction, bwt: TransformerFunction) = {
    val stOrig = parseWithGrammar(g)(i)
    val stTransformed = fwt(stOrig)
    val stBackwards = bwt(stTransformed.head)
    println
    println("Input parsed, then unparsed: ")
    println(stOrig.unparse)
    println
    println("Transformed: ")
    println(stTransformed.head.unparse)
    println
    println("Back again: ")
    println(stBackwards.head.unparse)
    println
  }
  
  def parse[T <: Parboiled2Parser[X], X](file: String)(implicit factory: (String) => T): X = {
    val fileHandle = scala.io.Source.fromFile(file)
    val fileContents = fileHandle.mkString
    fileHandle.close
    val resultHandler = factory(fileContents)
    resultHandler.InputFile.run() match {
      case Success(x) => x
      case Failure(e: ParseError) => {
        println("Expr is not valid: " + e.format(input = fileContents))
        showUsage
        sys.error("")
      }
      case Failure(e) => {
        println("Unknown error: " + e)
        showUsage
        sys.error("")
      }
    }
  }
  
  def showUsage: Unit = {
    println("Usage: Main <TransformInstructions.ti>")
  }
  
  /*
  def main(args: Array[String]): Unit = {
    
    
    val (gTrans, fwt, bwt) = transformGrammarWithFile(g1, "concreteToAbstract.tr")
    val stC = parseWithGrammar(g1)("2*[4+[3+[22*[11+[2*[3+4]]]]]]")
    val stA = fwt(stC)
    val stC2 = bwt(stA.head)
    println(stC.unparse)
    println(stA.head.unparse)
    println(stC2.head.unparse)
  }
  */
}