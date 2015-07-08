object Grammar {
	sealed trait GrammarAtom
	case class Nonterminal(sym: Symbol) extends GrammarAtom{
		override def toString = sym.name
    def asString(indent: String): String = indent + toString
	}
	case class Terminal(sym: String) extends GrammarAtom{
		override def toString = "\"" + sym + "\""
	}
	case class Regex(sym: String) extends GrammarAtom{
		override def toString = "\"" + sym + "\".r"
	}
	case object IntegerTerminal extends GrammarAtom{
		override def toString = "<int>"
	}
	
	case class GrammarRule(lhs: Nonterminal, rhs: List[GrammarAtom], tag: Int){
		def asString(indent: String, max: Int) = indent + padding(tag, max) + tag + " | " + lhs + " ->" + rhs.map(_.toString).fold("")(joinStringsBy(" "))
		override def toString = asString("", 0)
		var matched = false
	}
	type GrammarRules = List[GrammarRule]
	type RHS = List[GrammarAtom]
	
	case class Grammar(start: Nonterminal, rules: GrammarRules){
		def lookup(nonterminal: Nonterminal): GrammarRules = rules.filter({gr => gr.lhs.sym == nonterminal.sym})
		override def toString = "Grammar:" +
			"\n  Start: " + start +
			"\n  Rules:"  + rules.map(_.asString("    ", rules.length)).fold("")(joinStringsBy("\n")) + 
			"\n"
	}
	
	def joinStringsBy(join: String)(a: Any, b: Any) = a + join + b
	def padding(i: Int, n: Int) = " " * (n.toString.length - i.toString.length)
	
	sealed trait SyntaxTree
	case class Branch(nt: Symbol, childs: List[SyntaxTree]) extends SyntaxTree
	trait Leaf extends SyntaxTree
	case class LeafString(str: String) extends Leaf
	case class LeafInteger(i: Int) extends Leaf
	
	
	
	type Parser[A] = String => Option[(A, String)]
	def choice[A](firstTry: => Parser[A], secondTry: => Parser[A]): Parser[A] = input => {
		firstTry(input) orElse secondTry(input)
  }
    
  def sequence[A, B](parseFirstPart: => Parser[A], parseSecondPart: => Parser[B]): Parser[(A, B)] =
    input => parseFirstPart(input) match {
      case Some((firstResult, afterFirstPart)) => parseSecondPart(afterFirstPart) match {
          case Some((secondResult, afterSecondPart)) => Some( ((firstResult, secondResult), afterSecondPart) )
          case None => None
        }
      case None => None
    }
    
  def postprocess[A, B](parser: => Parser[A])(postprocessor: A => B): Parser[B] =
    input => parser(input) match {
      case Some( (result, rest) ) => Some( (postprocessor(result), rest) )
      case None => None
    }
	implicit class ParserOps[A](self: => Parser[A]) {
    def | (that: => Parser[A]): Parser[A] = choice(self, that)
    def ~ [B] (that: => Parser[B]): Parser[(A, B)] = sequence(self, that)
    def ^^ [B] (postprocessor: A => B): Parser[B] = postprocess(self)(postprocessor)
  }
  
  def listToOption[A](l: List[Parser[A]]): Parser[A] = l.reduceRight((x, y) => x | y)
  	
	def parseString(expected: String): Parser[String] = code => {
		println("T: trying to match: " + expected)
    println("T: with:\n" + code.split("\n")(0))
    if (code startsWith expected) Some((expected, code drop expected.length))
    else None
  }
  def parseRegex(regex: String): Parser[String] = code => {
    println("R: trying to match: '" + regex + "'")
    println("R: with:            '" + code + "'")
    val Pattern = s"(?s)($regex)(.*)".r      	
    code match {
      case Pattern(groups @ _*) => {
      	//println("R: matched:       '" + groups.head + "'")
      	Some((groups.head, groups.last))
      }
      case otherwise => {
      	None
      }
    }
  }
  def keywordParser(keyword: String): Parser[SyntaxTree] = 
    parseString(keyword) ^^ { x => LeafString(x) }
  
  def digitsParser: Parser[SyntaxTree] = 
    parseRegex("[0-9]+") ^^ { x => LeafInteger(x.toInt) }
    
  def regexParser(r: String): Parser[SyntaxTree] = 
    parseRegex(r) ^^ { x => LeafString(x) }
  
	def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[SyntaxTree] = {
		//(grammar lookup nonterminal).map(_.asString("", 1)).foreach(println)
		listToOption((grammar lookup nonterminal).map(parseRHS(_, grammar) ^^ {
      children => Branch(nonterminal.sym, children)
    }))
  }
	
	def parseRHS(rule: GrammarRule, grammar: Grammar): Parser[List[SyntaxTree]] = rule.rhs match {
		case head :: tail => {
			//println("rule: " + rule.tag + ": " + rule.asString("", 1))
			recurseParseRHS(head, GrammarRule(rule.lhs, tail, rule.tag), grammar)
		}
		case Nil => {s => Some((Nil, s))}
	}
	
	def recurseParseRHS(head: GrammarAtom, tail: GrammarRule, grammar: Grammar): Parser[List[SyntaxTree]] = { 
		parseGrammarAtom(head, grammar) ~ parseRHS(tail, grammar) ^^ {
			case (t1, t2) => t1 :: t2
		}
	}
	
	def parseGrammarAtom(head: GrammarAtom, grammar: Grammar): Parser[SyntaxTree] = head match {
				case nt: Nonterminal => parseNonterminal(nt, grammar)
				case Terminal(str) => keywordParser(str)
				case Regex(str) => regexParser(str)
				case IntegerTerminal => digitsParser
			}
			
	def parseWithGrammar(g: Grammar)(str: String): SyntaxTree = {
		var parser = parseNonterminal(g.start, g)
    parser(str) match {
      case Some((t, rest)) => {
      	if (rest == "") t
      	else {
      		println("Not matched!!\n--------\n" + rest + "\n--------\n")
      		t
      	}
      }
      case None => sys.error("Not an Expression: " + str)
      //case None => Branch(Symbol(""), List())
   	}
  }
}