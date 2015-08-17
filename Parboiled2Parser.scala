trait Parboiled2Parser[+X] extends org.parboiled2.Parser {
  def InputFile: org.parboiled2.Rule1[X]
}