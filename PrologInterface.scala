class PrologInterface {
  import org.jpl7.{Integer => pInteger, Float => pFloat, _}
  import java.io._
  import java.nio.file.{Paths, Files}
  import Transform._
  import util.Random
  import PrologInterface._
  
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
    
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    definitionsToWrite.map(_.toString).map(bw.write)
    bw.close()
    
    loadPLFile(fileName)
  }
  
  def addDefinition(d: Definition) = (definitionsToWrite = d :: definitionsToWrite)
  

}
object PrologInterface {
  import org.jpl7.{Integer => pInteger, Float => pFloat, _}
  
  case class Definition(lhs: Term, rhs: List[Term]){
    override def toString = lhs + (if(rhs.length > 0) " :-\n\t" else "") + rhs.mkString(",\n\t") + "."
  }
  
  
}