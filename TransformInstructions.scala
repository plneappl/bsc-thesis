import org.parboiled2._
import shapeless._

case class TransformInstructionsFile(grammar: String, transformer: String, input: String)

class TransformInstructions(val input: ParserInput) extends Parboiled2Parser[TransformInstructionsFile] {
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace) ~ optional("//" ~ zeroOrMore(!t_newLine ~ ANY)) ~ oneOrMore(t_newLine) ~ t_optspace }
  
  def InputFile = rule {
    t_literal ~ commentNL ~
    t_literal ~ commentNL ~
    t_literal ~ optional(commentNL) ~> ((s1, s2, s3) => TransformInstructionsFile(s1, s2, s3))
    
  }
    
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }
  def t_newLine    = CharPredicate("\r\n")
  def t_literal          = rule { 
    ("\"" ~ capture(optional(zeroOrMore(!CharPredicate("\"\n\r") ~ ANY))) ~ "\"") 
  }  
}
