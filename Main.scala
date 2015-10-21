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
      var gTrans: GrammarCC = inputGrammar
      var fwt: TransformerFunction = List(_)
      var bwt: TransformerFunction = List(_)
      tif.commands.foreach {
        case parseWithOriginal(input) => parseTest(inputGrammar, input, fwt, bwt)
        case parseWithTransformed(input) => parseTest(gTrans, input, bwt, fwt)
        case writeGrammar(file) => writeFile(file, gTrans.toString)
        case transformGrammarCommand(file) => {
          val (gTrans2, fwt2, bwt2) = transformGrammarWithFile(gTrans, file, maxDepth = Some(80), keepFile = false)
          gTrans = gTrans2
          fwt = fwt andThen ((_:TransformationResult).head) andThen fwt2
          bwt = bwt andThen ((_:TransformationResult).head) andThen bwt2
        }
        case exhaustivelyTransformGrammar(file, limit) => {
          val (gTrans2, fwt2, bwt2) = exhaustivelyTransform(inputGrammar, file, limit)
          gTrans = gTrans2
          fwt = fwt2
          bwt = bwt2
          println(limit)
        }
        case x => println(x)
      }
    }
  }
  
  def exhaustivelyTransform(g: GrammarCC, file: String, limit: Option[Int]): (GrammarCC, TransformerFunction, TransformerFunction) = {
    val transformMethod: GrammarCC => (GrammarCC, TransformerFunction, TransformerFunction) = transformGrammarWithFile(_, file, maxDepth = Some(80), keepFile = false)
    var (gTrans, fwt, bwt) = transformMethod(g)
    var cont = true
    var runs = 1
    do {
      runs = runs + 1
      //println("intermediate grammar: ")
      //println(gTrans)
      //println
      val (gTrans2, fwt2, bwt2) = transformMethod(gTrans)
      
      if(gTrans.equalRules(gTrans2.rules) || (limit.isDefined && limit.get < runs)) cont = false
      else {
        gTrans = gTrans2
        fwt = fwt andThen ((_:TransformationResult).head) andThen (x => {println(x); x}) andThen fwt2
        bwt = bwt2 andThen ((_:TransformationResult).head) andThen (x => {println(x); x}) andThen bwt
      }
    } while(cont)
    (gTrans, fwt, bwt)
  }
  
  def parseTest(g: GrammarCC, i: String, fwt: TransformerFunction, bwt: TransformerFunction) = {
    val stOrig = parseWithGrammar(g)(i)
    println
    println("Input parsed:\n----------------")
    println(stOrig)
    println(stOrig.latexTree)
    println
    val stTransformed = fwt(stOrig)
    println("Transformed:\n----------------")
    println(stTransformed.head)
    println(stTransformed.head.latexTree)
    println
    val stBackwards = bwt(stTransformed.head)
    println("Back again:\n----------------")
    println(stBackwards.head)
    println(stBackwards.head.latexTree)
    println
    println("Unparsed:")
    println(stOrig.unparse)
    println
    println("Transformed: ")
    println(stTransformed.head.unparse)
    println
    println("Back again: ")
    println(stBackwards.head.unparse)
    //stBackwards.map(_.unparse).foreach(println)
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
  
}