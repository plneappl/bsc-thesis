class PrologInterface {
  import org.jpl7.{Integer => pInteger, Float => pFloat, _}
  import FileIO._
  import Transform._
  import Grammar._
  import PrologInterface._
  import ReadableSyntaxGrammar.RuleName
  
  
  def loadPLFile(s: String) = {
    
    (new Query("consult", Array[Term](new Atom(s)))).oneSolution
  }
  
  var definitionsToWrite: List[Definition] = List()
  
  //c2a(...) :- c2a(...)
  
  def initializeDefinitions = (definitionsToWrite = List())
  
  var file: java.io.File = null
  def loadDefinitions = {
    
    file = writeTempFile(iterativeDeepening + "\n" + definitionsToWrite.sortBy(_.lhs.toString).mkString("\n"))
    
    loadPLFile(file.toString)
    
    
  }
  
  def addDefinition(d: Definition) = (definitionsToWrite = d :: definitionsToWrite)
  

  def unload(keepFile: Boolean = false) = {
    (new Query("unload_file", Array[Term](new Atom(file.toString)))).oneSolution
    if(!keepFile)
      file.delete
  }
  //TODO: A <- { ... }, _X, exhaustive, _X+ _X*
  
  
  //transforms a tree of g1 to the equivalent of g2
  def transformTree(
    t: SyntaxTree, 
    g1: Grammar, 
    g2: Grammar, 
    nonstop: Boolean = false, 
    maxDepth: Option[Int] = None,
    backwards: Boolean = false,
    startDepth: Int = 0): List[SyntaxTree] = {
    //println("\nInput tree:")
    //println("---------------")
    //println(t)
    //println("\nTransforming...")
    val X = new Variable("X")
    //println(treeToTerm(t))
    var limit = List(t.depth, startDepth).max
    if(!maxDepth.isEmpty)
      limit = List(limit, maxDepth.get).min
    //println(limit)
    var ret = List[SyntaxTree]()
    val relationArguments = if(backwards) Array[Term](X, treeToTerm(t))
      else Array[Term](treeToTerm(t), X)
    

    var continue = true
    do {
      val q = new Query("iterative", Array[Term](new Compound(tpRelName(g1.start, g2.start, flip = backwards), relationArguments), new pInteger(limit)))
      println(q)
      val sols = q.allSolutions
      //sols foreach println
      ret = sols.map(sol => termToTree(sol.get("X"), sol)).flatten.toList
      limit = limit + 2
      if(ret.isEmpty && !nonstop){
        println("Haven't found anything. Setting limit = " + limit + ". Continue? [Y/n]")
        var line: String = null
        while(line == null) line = readLine
        if(line == "n") continue = false
      } else if(!ret.isEmpty) continue = false
      else {
        println("Haven't found anything. Setting limit = " + limit + ".")
      }
      if(!maxDepth.isEmpty && limit > maxDepth.get) continue = false
    } while(continue)
    //println("\nTransformed:")
    //println("---------------")
    //ret foreach println
    //println(ret.head)
    ret.sortBy(t => (t.depth, t.unparse.length, t.unparse))
  }
  
  
  def treeToTerm(t: SyntaxTree): Term = {
    t match {
      case Branch(rn, childs) => new Compound("c" + rn, childs.map(treeToTerm).toArray)
      case LeafString(s) => new Atom(s)
      case LeafInteger(i) => new pInteger(i)
      case LeafFloat(f) => new pFloat(f)
    }
  }

  def termToTree(t: Term, sol: java.util.Map[String, Term]): Option[SyntaxTree] = {
    //println(t)
    t match {
      case t1:Variable => termToTree(sol.get(t), sol)
      case t1:Compound => {
        val rns = t.name.tail.split("_")
        val rn = RuleName(Nonterminal(rns(0)), rns(1))
        val childs = t.args.map(termToTree(_, sol)).flatten.toList
        Some(Branch(rn, childs))
      }
      case t1:Atom => Some(LeafString(t.name))
      case t1:pInteger => Some(LeafInteger(t.intValue))
      case t1:pFloat => Some(LeafString(t.toString))
      case _ => None
    }
  }
}




object PrologInterface {
  import org.jpl7.{Integer => pInteger, Float => pFloat, _}
  import Grammar._
  import scala.util.{Failure, Success}
  import org.parboiled2._
  import Transform._
  

  val iterativeDeepening = 
"""clause_tree(true,_,_) :- !.
clause_tree(_,D,Limit) :- D > Limit,
                          !,
                          fail.  %% reached depth limit
clause_tree((A,B),D,Limit) :- !,
                              clause_tree(A,D,Limit),
                              clause_tree(B,D,Limit).
clause_tree(A,_,_) :- predicate_property(A,built_in),
                      !,
                      call(A).
clause_tree(A,D,Limit) :- clause(A,B),
                          D1 is D+1,
                          clause_tree(B,D1,Limit).

iterative(G,D) :- clause_tree(G,0,D).


"""

  //HACK to load JPL in Linux
  def unsafeAddDir(dir: String) = try {
    val field = classOf[ClassLoader].getDeclaredField("usr_paths")
    field.setAccessible(true)
    val paths = field.get(null).asInstanceOf[Array[String]]
    if(!(paths contains dir)) {
      field.set(null, paths :+ dir)
      System.setProperty("java.library.path",
       System.getProperty("java.library.path") +
       java.io.File.pathSeparator +
       dir)
    }
  } catch {
    case _: IllegalAccessException =>
      sys.error("Insufficient permissions; can't modify private variables.")
    case _: NoSuchFieldException =>
      sys.error("JVM implementation incompatible with path hack")
  }
  //HACK end

  case class Definition(lhs: Term, rhs: List[Term]){
    override def toString = lhs + (if(rhs.length > 0) " :-\n\t" else "") + rhs.mkString(",\n\t") + "."
  }
  
  
  type TransformationResult = List[SyntaxTree]
  type TransformerFunction = (SyntaxTree => TransformationResult)
  
  //returns the transformed grammar and the forwards- and the backwardstransformation.
  def transformGrammarWithFile(
    g: Grammar, 
    filePath: String, 
    keepFile: Boolean = false, 
    maxDepth: Option[Int] = None): (Grammar, TransformerFunction, TransformerFunction) = {
    val source = scala.io.Source.fromFile(filePath)
    val tr = source.mkString
    source.close
    val a = new ReadableSyntaxGrammar(tr)
    a.InputFile.run() match {
      case Success(exprAst) => {
        val (gTrans, psns, defs) = applyTransformerFile(g)(exprAst)
        println("Transformer:\n------------\n" + exprAst + "\n---------------\n\n")
        println("Starting Grammar:\n------------\n" + g + "\n---------------\n\n")
        println("Transformed Grammar:\n------------")
        println(gTrans)
        println("\nPatterns:\n------------")
        psns foreach println
        println("\nDefinitions:\n---------------")
        defs foreach println
        println
        val pli = new PrologInterface
        
        defs foreach pli.addDefinition
        
        val fwt = ((s: SyntaxTree) => {
          pli.loadDefinitions
          val t = pli.transformTree(s, g, gTrans, maxDepth = maxDepth)
          pli.unload(keepFile = keepFile)
          t
        })

        val bwt = ((s: SyntaxTree) => {
          pli.loadDefinitions
          val t = pli.transformTree(s, gTrans, g, maxDepth = maxDepth, backwards = true)
          pli.unload(keepFile = keepFile)
          t
        })
        
        (gTrans, fwt, bwt)
      
      }
      case Failure(e: ParseError) => sys.error("Expr is not valid: " + e.format(tr))
      case Failure(e) => sys.error("Unknown error: " + e)
    }
  }
  
}