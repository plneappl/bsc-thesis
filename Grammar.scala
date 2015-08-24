object Grammar {  
  
  import ReadableSyntaxGrammar.RuleName
  
  
	sealed trait GrammarAtom { def bare: String }
	case class Nonterminal(sym: Symbol) extends GrammarAtom {
		override def toString = sym.name
    def asString(indent: String): String = indent + toString
    def bare = toString
	}
  object Nonterminal {
    def apply(s: String): Nonterminal = new Nonterminal(Symbol(s))
  }
  
  case class GrammarAtomSequence(seq: List[GrammarAtom]) extends GrammarAtom {
    override def toString = seq.mkString(" ")
    def bare = seq.map(_.bare).mkString(" ")
  }

  sealed trait TerminalLike extends GrammarAtom
  
	case class Terminal(sym: String) extends TerminalLike {
		override def toString = "\"" + sym + "\""
    def bare = sym
	}
	case class Regex(sym: String) extends TerminalLike {
		override def toString = "\"" + sym + "\".r"
    def bare = sym
	}
	case object IntegerTerminal extends TerminalLike {
		override def toString = "<int>"
    def bare = "int"
	}
  case object FloatTerminal extends TerminalLike {
    override def toString = "<float>"
    def bare = "float"
  }
	
	case class GrammarRule(lhs: Nonterminal, rhs: List[GrammarAtom], tag: String){
		def asString(indent: String, max: Int) = indent + padding(tag, max) + lhs + " -> " + ruleName + " " + rhs.map(_.toString).mkString(" ")
		override def toString = asString("", 0)
		var matched = false
    def ruleName = RuleName(lhs, tag)
	}
	type GrammarRules = List[GrammarRule]
	type RHS = List[GrammarAtom]
	
	case class Grammar(start: Nonterminal, rules: GrammarRules){
		def lookup(nonterminal: Nonterminal): GrammarRules = rules.filter({gr => gr.lhs.sym == nonterminal.sym})
		override def toString = "start " + start + "\n" + 
    rules.groupBy(_.lhs).toList.map(lhsRules => {
      val lhs = lhsRules._1
      val rules1 = lhsRules._2
      "  " + lhs + " -> " + rules1.head.ruleName + " " + rules1.head.rhs.mkString(" ") + 
      rules1.tail.map(rule => {
        "\n" + (" " * lhs.toString.length) + "    | " + rule.ruleName + " " + 
        rule.rhs.mkString(" ")}).mkString 
    }).mkString("\n\n") + 
    "\n"
    def equalRules(r: GrammarRules) = r.toSet == rules.toSet
	}
	
	def joinStringsBy(join: String)(a: Any, b: Any) = a + join + b
	def padding(i: String, n: Int) = " " * (n.toString.length - i.length)
	
	sealed trait SyntaxTree{
    def asString(indent: String): String
    override def toString = asString("")
    def depth: Int
    def unparse: String
  }
	case class Branch(rn: RuleName, childs: List[SyntaxTree]) extends SyntaxTree {
    def asString(indent: String) = indent + "Branch: " + rn + "\n" + 
      childs.map(_.asString(indent + "  ")).mkString("\n")
    def depth = childs.map(_.depth).max + 1
    def unparse = childs.map(_.unparse).mkString("")
  }
	sealed trait Leaf extends SyntaxTree{
    def depth = 1
  }
	case class LeafString(str: String) extends Leaf {
    def asString(indent: String) = indent + "StrLeaf: " + str
    def unparse = str
  }
	case class LeafInteger(i: Int) extends Leaf {
    def asString(indent: String) = indent + "IntLeaf: " + i
    def unparse = i.toString
  }
  case class LeafFloat(f: Double) extends Leaf {
    def asString(indent: String) = indent + "FloatLeaf: " + f
    def unparse = f.toString
  }


  trait Parser[A] {
    def allResults(code: String): Iterator[(A, String)]

    def apply(code: String): Option[(A, String)] = {
      val results = allResults(code)
      if (results.nonEmpty)
        Some(results.next())
      else
        None
    }

    def | (that: => Parser[A]): Parser[A] = new ChoiceParser(this, that)
    def ~ [B] (that: => Parser[B]): Parser[(A, B)] = new SequenceParser(this, that)
    def ^^ [B] (postprocessor: A => B): Parser[B] = new Postprocessor(this, postprocessor)

  }

  class ChoiceParser[A](
    firstTry: => Parser[A],
    secondTry: => Parser[A]
  ) extends Parser[A] {
    def allResults(code: String): Iterator[(A, String)] =
      firstTry.allResults(code) ++ secondTry.allResults(code)
  }

  object ChoiceParser {
    def apply[A](parsers: List[Parser[A]]): Parser[A] =
      parsers.reduceRight((x, y) => x | y)
  }

  class SequenceParser[A, B](
    firstPart: => Parser[A],
    secondPart: => Parser[B]
  ) extends Parser[(A, B)] {
    def allResults(code: String): Iterator[((A, B), String)] =
      for {
        (lhs, rhsCode)  <- firstPart.allResults(code)
        (rhs, leftover) <- secondPart.allResults(rhsCode)
      }
      yield ((lhs, rhs), leftover)
  }

  class Postprocessor[A, B](parser: => Parser[A], postprocessor: A => B) extends Parser[B] {
    def allResults(code: String): Iterator[(B, String)] =
      parser.allResults(code).map {
        case (result, leftover) => (postprocessor(result), leftover)
      }
  }

  case class StringParser(expected: String) extends Parser[String] {
    def allResults(code: String): Iterator[(String, String)] =
      if (code startsWith expected)
        Iterator((expected, code drop expected.length))
      else
        Iterator.empty
  }

  case class RegexParser(regex: String) extends Parser[String] {
    def allResults(code: String): Iterator[(String, String)] = {
      val Pattern = s"(?s)($regex)(.*)".r
      code match {
        case Pattern(groups @ _*) => {
          Iterator((groups.head, groups.last))
        }
        case otherwise => {
          Iterator.empty
        }
      }
    }
  }

  case class NothingParser[A](result: A) extends Parser[A] {
    def allResults(code: String): Iterator[(A, String)] =
      Iterator((result, code))
  }

  def keywordParser(keyword: String): Parser[SyntaxTree] = 
    StringParser(keyword) ^^ { x => LeafString(x) }
  
  def digitsParser: Parser[SyntaxTree] = 
    RegexParser("[0-9]+") ^^ { x => LeafInteger(x.toInt) }

  def floatParser: Parser[SyntaxTree] = 
    RegexParser("[0-9]+\\.[0-9]+") ^^ {x => LeafFloat(x.toDouble) }
    
  def regexParser(r: String): Parser[SyntaxTree] = 
    RegexParser(r) ^^ { x => LeafString(x) }

  def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[SyntaxTree] = {
    ChoiceParser((grammar lookup nonterminal).map(r => parseRHS(r.rhs, grammar) ^^ {
      children => Branch(r.ruleName, children): SyntaxTree
    }))
  }

  def parseRHS(rule: List[GrammarAtom], grammar: Grammar): Parser[List[SyntaxTree]] = rule match {
    case head :: tail => {
      //println("rule: " + rule.tag + ": " + rule.asString("", 1))
      recurseParseRHS(head, tail, grammar)
    }
    case Nil => NothingParser(Nil)
  }

  def recurseParseRHS(head: GrammarAtom, tail: List[GrammarAtom], grammar: Grammar): Parser[List[SyntaxTree]] = { 
    parseGrammarAtom(head, grammar) ~ parseRHS(tail, grammar) ^^ {
      case (t1, t2) => t1 :: t2
    }
  }
  
  def parseGrammarAtom(head: GrammarAtom, grammar: Grammar): Parser[SyntaxTree] = head match {
    case nt: Nonterminal => parseNonterminal(nt, grammar)
    case Terminal(str) => keywordParser(str)
    case Regex(str) => regexParser(str)
    case IntegerTerminal => digitsParser
    case FloatTerminal => floatParser
    case GrammarAtomSequence(l) => parseRHS(l, grammar) ^^ { l1 => Branch(RuleName(Nonterminal(""), ""), l1) }
  }
      
  def parseWithGrammar(g: Grammar)(str: String): SyntaxTree = {
    val results = parseNonterminal(g.start, g).allResults(str)
    if (results.nonEmpty) {
      val (t, rest) = results.minBy(_._2.length)
      if (rest == "") t
      else {
        println("Not matched!!\n--------\n" + rest + "\n--------\n")
        t
      }
    }
    else
      sys.error("Not an Expression: " + str)
  }

}
