object FileIO {
  import java.io._
  import java.nio.file.{Paths, Files}
  import util.Random
    
  def writeFile(fileName: String, content: String): Unit = writeFile(new File(fileName), content)
  def writeFile(file: File, content: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close
  }
  
  def writeTempFile(content: String): File = {
    var fileName = ""
    do {
      fileName = "temp_" + Random.alphanumeric.take(10).mkString + ".pl"
    } while(Files.exists(Paths.get(fileName)))
    println("tempfile: " + fileName)
    val file = new File(fileName)
    writeFile(file, content)
    file
  }
  
  def readFile(fileName: String): String = {
    val fileHandle = scala.io.Source.fromFile(fileName)
    val fileContents = fileHandle.mkString
    fileHandle.close
    fileContents
  }
  
}