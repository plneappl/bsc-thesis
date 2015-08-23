object Main {
  import PrologInterface._
  import scala.util.{Failure, Success}
  import Grammar.{parseWithGrammar, SyntaxTree, Grammar => GrammarCC}
  import org.parboiled2._
  import FileIO._
  import TransformInstructions._
  
  
  
  
  def main(args: Array[String]): Unit = {
    if(args.length == 0){
      showUsage
    } 
    else{
      //HACKY load JPL on Linux  
      if(!System.getProperty("os.name").startsWith("Windows"))
        unsafeAddDir("/usr/lib/swi-prolog/lib/amd64")
      //end HACK
      
      
      val tif = parse[TransformInstructions, TransformInstructionsFile](args(0))(s => new TransformInstructions(s))
      val inputGrammar = parse[GrammarGrammar, GrammarCC](tif.grammar)(s => new GrammarGrammar(s))
      println(tif)
      var gTrans: GrammarCC = null
      var fwt: TransformerFunction = null
      var bwt: TransformerFunction = null
      tif.commands.foreach {
        case parseWithOriginal(input) => parseTest(inputGrammar, input, fwt, bwt)
        case parseWithTransformed(input) => parseTest(gTrans, input, bwt, fwt)
        case writeGrammar(file) => writeFile(file, gTrans.toString)
        case transformGrammarCommand(file) => {
          val (gTrans2, fwt2, bwt2) = transformGrammarWithFile(inputGrammar, file, maxDepth = Some(80), keepFile = false)
          gTrans = gTrans2
          fwt = fwt2
          bwt = bwt2
          println("transformed.")
        }
        case exhaustivelyTransformGrammar(file) => {
          val (gTrans2, fwt2, bwt2) = exhaustivelyTransform(inputGrammar, file)
          gTrans = gTrans2
          fwt = fwt2
          bwt = bwt2
        }
        case x => println(x)
      }
    }
  }
  
  def exhaustivelyTransform(g: GrammarCC, file: String): (GrammarCC, TransformerFunction, TransformerFunction) = {
    var (gTrans, fwt, bwt) = transformGrammarWithFile(g, file, maxDepth = Some(80), keepFile = false)
    var cont = true
    do {
      println("intermediate grammar: ")
      println(gTrans)
      println
      val (gTrans2, fwt2, bwt2) = transformGrammarWithFile(gTrans, file, maxDepth = Some(80), keepFile = false)
      
      if(gTrans2.rules.length < gTrans.rules.length) cont = false
      else{
        gTrans = gTrans2
        fwt = fwt andThen ((_:TransformationResult).head) andThen fwt2
        bwt = bwt andThen ((_:TransformationResult).head) andThen bwt2
      }
    } while(cont)
    (gTrans, fwt, bwt)
  }
  
  def parseTest(g: GrammarCC, i: String, fwt: TransformerFunction, bwt: TransformerFunction) = {
    val stOrig = parseWithGrammar(g)(i)
    println
    println("Input parsed: ")
    println(stOrig)
    println
    val stTransformed = fwt(stOrig)
    println("Transformed: ")
    println(stTransformed.head)
    println
    val stBackwards = bwt(stTransformed.head)
    println("Back again: ")
    println(stBackwards.head)
    println
    println("Unparsed:")
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
    val fileContents = readFile(file)
    
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