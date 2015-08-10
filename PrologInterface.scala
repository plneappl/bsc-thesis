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
    definitionsToWrite.map(_.toString).map(bw.write)
    bw.close
    
    loadPLFile(fileName)
    
    //file.delete
  }
  
  def addDefinition(d: Definition) = (definitionsToWrite = d :: definitionsToWrite)
  
  def transformTree(t: SyntaxTree, g1: Grammar, g2: Grammar): Array[SyntaxTree] = {
    val X = new Variable("X")
    println(treeToTerm(t))
    val q = new Query(tpRelName(g1.start, g2.start), Array[Term](treeToTerm(t), X))
    val sols = q.allSolutions
    //sols foreach println
    sols.map(sol => termToTree(sol.get("X"), sol)).flatten
  }
  
  def treeToTerm(t: SyntaxTree): Term = {
    //println(t)
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
  
  case class Definition(lhs: Term, rhs: List[Term]){
    override def toString = lhs + (if(rhs.length > 0) " :-\n\t" else "") + rhs.mkString(",\n\t") + "."
  }
  
  
}