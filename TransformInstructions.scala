import org.parboiled2._
import shapeless._

case class TransformInstructionsFile(grammar: String, transformer: String, commands: List[Command])

class TransformInstructions(val input: ParserInput) extends Parboiled2Parser[TransformInstructionsFile] {
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace) ~ optional("//" ~ zeroOrMore(!t_newLine ~ ANY)) ~ oneOrMore(t_newLine) ~ t_optspace }
  
  def InputFile = rule {
    t_literal ~ commentNL ~
    t_literal ~ 
    zeroOrMore(commentNL ~ command) ~
    optional(commentNL) ~> ((s1: String, s2: String, s3: Seq[Command]) => TransformInstructionsFile(s1, s2, s3.toList))
    
  }
  
  def command = rule {(
      ("gOrig(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> parseWithOriginal)    
    | ("gTran(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> parseWithTransformed)   
    | ("writeGrammar(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> writeGrammar)
  )}  
    
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }
  def t_newLine    = CharPredicate("\r\n")
  def t_literal          = rule { 
    ("\"" ~ capture(optional(zeroOrMore(!CharPredicate("\"\n\r") ~ ANY))) ~ "\"") 
  }  
}

sealed trait Command
case class parseWithOriginal(input: String) extends Command
case class parseWithTransformed(input: String) extends Command
case class writeGrammar(file: String) extends Command
