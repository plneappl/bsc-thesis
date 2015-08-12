class PrologInterface {
  import org.jpl7.{Integer => pInteger, Float => pFloat, _}
  import java.io._
  import java.nio.file.{Paths, Files}
  import Transform._
  import Grammar._
  import util.Random
  import PrologInterface._
  import ReadableSyntaxGrammar.RuleName
  
  def loadPLFile(s: String) = {
    
    (new Query("consult", Array[Term](new Atom(s)))).oneSolution
  }
  
  var definitionsToWrite: List[Definition] = List()
  
  //c2a(...) :- c2a(...)
  
  def initializeDefinitions = (definitionsToWrite = List())
  
  def loadDefinitions = {
    var fileName = ""
    do {
      fileName = "temp_" + Random.alphanumeric.take(10).mkString + ".pl"
    } while(Files.exists(Paths.get(fileName)))
    println("tempfile: " + fileName)
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(iterativeDeepening)
    definitionsToWrite.map(_.toString.replace("+", "plus")).map(x=>{bw.write(x); bw.write("\n")})
    bw.close
    
    loadPLFile(fileName)
    
    //file.delete
  }
  
  def addDefinition(d: Definition) = (definitionsToWrite = d :: definitionsToWrite)
  
  //transforms a tree of g1 to the equivalent of g2
  def transformTree(t: SyntaxTree, g1: Grammar, g2: Grammar): List[SyntaxTree] = {
    val X = new Variable("X")
    //println(treeToTerm(t))
    var limit = 2
    var ret = List[SyntaxTree]()
    var continue = true
    do {
      val q = new Query("iterative", Array[Term](new Compound(tpRelName(g1.start, g2.start), Array[Term](treeToTerm(t), X)), new pInteger(limit)))
      //println(q)
      val sols = q.allSolutions
      //sols foreach println
      ret = sols.map(sol => termToTree(sol.get("X"), sol)).flatten.toList
      limit = limit + 2
      if(ret.isEmpty){
        println("Haven't found anything. Setting limit = " + limit + ". Continue? [Y/n]")
        var line: String = null
        while(line == null) line = readLine
        if(line == "n") continue = false
      } else continue = false
    } while(continue)
    ret
  }
  
  def treeToTerm(t: SyntaxTree): Term = {
    t match {
      case Branch(rn, childs) => new Compound("c" + rn, childs.map(treeToTerm).toArray)
      case LeafString(s) => new Atom(s)
      case LeafInteger(i) => new pInteger(i)
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
      error("Insufficient permissions; can't modify private variables.")
    case _: NoSuchFieldException =>
      error("JVM implementation incompatible with path hack")
  }
  //HACK end

  case class Definition(lhs: Term, rhs: List[Term]){
    override def toString = lhs + (if(rhs.length > 0) " :-\n\t" else "") + rhs.mkString(",\n\t") + "."
  }
  
  
}