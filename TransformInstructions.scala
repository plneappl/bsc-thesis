import org.parboiled2._
import shapeless._


class TransformInstructions(val input: ParserInput) extends Parboiled2Parser[TransformInstructions.TransformInstructionsFile] {
  import TransformInstructions._
  def wspStr(s: String): Rule0 = rule {
    str(s) ~ t_optspace
  }
  
  def commentNL    = rule { quiet(oneOrMore(commentEOL)) }
  def commentEOL   = rule { optional(t_optspace) ~ optional(comment) ~ oneOrMore(t_newLine) ~ t_optspace }
  def comment      = rule { "//" ~ zeroOrMore(!t_newLine ~ ANY) }
  
  
  def InputFile = rule {
    t_literal ~ 
    zeroOrMore(commentNL ~ command) ~
    optional(commentNL) ~ optional(comment) ~ EOI ~> ((s1: String, s2: Seq[Command]) => TransformInstructionsFile(s1, s2.toList))
    
  }
  
  def command = rule {(
      ("trans(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> transformGrammarCommand)
    | ("exhst(" ~ t_optspace ~ t_literal ~ t_optspace ~ optional(t_comma ~ t_optspace ~ t_num ~> (_.toInt)) ~ ")" ~> exhaustivelyTransformGrammar)
    | ("gOrig(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> parseWithOriginal)    
    | ("gTran(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> parseWithTransformed)   
    | ("writeGrammar(" ~ t_optspace ~ t_literal ~ t_optspace ~ ")" ~> writeGrammar)
  )}  
    
  def t_optspace         = rule { zeroOrMore(CharPredicate(" \t")) }
  def t_comma            = CharPredicate(",")
  def t_num              = rule { capture(oneOrMore(CharPredicate.Digit)) }
  def t_newLine    = CharPredicate("\r\n")
  def t_literal          = rule { 
    ("\"" ~ capture(optional(zeroOrMore(!CharPredicate("\"\n\r") ~ ANY))) ~ "\"") 
  }  
}

object TransformInstructions{
  sealed trait Command
  case class transformGrammarCommand(file: String) extends Command
  case class exhaustivelyTransformGrammar(file: String, limit: Option[Int]) extends Command
  case class parseWithOriginal(input: String) extends Command
  case class parseWithTransformed(input: String) extends Command
  case class writeGrammar(file: String) extends Command
  
  case class TransformInstructionsFile(grammar: String, commands: List[Command])
}
