object Transform {
	
	import sext._
	
	sealed trait GrammarAtom
	case class Nonterminal(sym: Symbol) extends GrammarAtom
	case class Terminal(sym: String) extends GrammarAtom
	case object IntegerTerminal extends GrammarAtom
	
	//todo: 
	/*
	- produce patterns (?)
	*/
	case class GrammarRule(lhs: Nonterminal, rhs: List[GrammarAtom], tag: Int)
	type GrammarRules = List[GrammarRule]
	
	case class Grammar(start: Nonterminal, rules: GrammarRules){
		def lookup(nonterminal: Nonterminal): List[List[GrammarAtom]] = rules.filter({gr => gr.lhs.sym == nonterminal.sym}).map(r => r.rhs)
	}
	
	val c = Nonterminal('C)
	val a = Nonterminal('A)
	val s = Nonterminal('S)
	val f = Nonterminal('F)
	
	val leftBrace  = Terminal("[")
	val rightBrace = Terminal("]")
	val plus       = Terminal("+")
	val mul        = Terminal("*")
	
	//concrete grammar
	val rules1 = List(
		GrammarRule(c, List(s, plus, c), 1),
		GrammarRule(c, List(s), 2),
		GrammarRule(s, List(f, mul, s), 3),
		GrammarRule(s, List(f), 4),
		GrammarRule(f, List(IntegerTerminal), 5),
		GrammarRule(f, List(leftBrace, c, rightBrace), 6)
		)
	
	val g1 = Grammar(c, rules1)
	
	//abstract grammar
	val rules2 = List(
		GrammarRule(a, List(a, plus, a), 1),
		GrammarRule(a, List(a, mul, a), 2),
		GrammarRule(a, List(IntegerTerminal), 3)
		)
	val g2 = Grammar(a, rules2)
	
	sealed abstract class TransformerAtom{def tag: Int}
	case class NonterminalMatcher(id: Int, tag: Int) extends TransformerAtom
	case class TerminalMatcher(matches: String, tag: Int) extends TransformerAtom
	case class IntegerMatcher(tag: Int) extends TransformerAtom
	
	type TransformerSequence = List[TransformerAtom] 
	case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence)
	case class TransformerRule(from: List[GrammarRuleMatcher], to: List[GrammarRuleMatcher])
	type TransformerRules = List[TransformerRule]
	case class GrammarTransformer(start: NonterminalMatcher, rules: TransformerRules)
	
	//C to A: take C -> C + S, C -> S and produce A -> A + A
	val transformCtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(1, 0), List(NonterminalMatcher(2, 1), TerminalMatcher("+", 2), NonterminalMatcher(1, 3))),
			GrammarRuleMatcher(NonterminalMatcher(1, 0), List(NonterminalMatcher(2, -1)))
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher(0, 0), List(NonterminalMatcher(0, 1), TerminalMatcher("+", 2), NonterminalMatcher(0, 3)))
		)
	)
	
	//S to A: take S -> S * F, S -> F and produce A -> A * A
	val transformStoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(2, 0), List(NonterminalMatcher(3, 1), TerminalMatcher("*", 2), NonterminalMatcher(2, 3))),
			GrammarRuleMatcher(NonterminalMatcher(2, 0), List(NonterminalMatcher(3, -1)))
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher(0, 0), List(NonterminalMatcher(0, 1), TerminalMatcher("*", 2), NonterminalMatcher(0, 3)))
		)
	)

	
	//F to A: take F -> [ C ], F -> int and produce A -> int
	val transformFtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(3, 0), List(TerminalMatcher("[", -1), NonterminalMatcher(1, -1), TerminalMatcher("]", -1))),
			GrammarRuleMatcher(NonterminalMatcher(3, 0), List(IntegerMatcher(1)))	
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher(0, 0), List(IntegerMatcher(1)))
		)
	)
	
	//NonterminalMatchers can identify themselves with any symbol, this stores the identification while transforming
	type SymbolTable = Map[Int, Symbol]
	
	//insert unknown symbols and check known for equality, fail if not equal
	def checkSymbolTable(st: SymbolTable, sym: Symbol, id: Int): SymbolTable = st.get(id) match {
		case Some(sym2) if(sym2 == sym) => st
		case None => st + ((id, sym))
	}
		
	//match a grammar rule with a matcher one atom at a time	
	def matches(st: SymbolTable, grammarRule: GrammarRule, matcher: GrammarRuleMatcher): Option[SymbolTable] = if(grammarRule.rhs.size == matcher.rhs.size) try {
		var symbolTable = checkSymbolTable(st, grammarRule.lhs.sym, matcher.lhs.id)
		for((ga, ma) <- (grammarRule.rhs zip matcher.rhs)) (ga, ma) match {
			case (Nonterminal(sym), NonterminalMatcher(id, _)) => symbolTable = checkSymbolTable(st, sym, id)
			case (Terminal(str1), TerminalMatcher(str2, _)) if (str1 == str2)	=> {}
			case (IntegerTerminal, IntegerMatcher(_)) => {}
			case _ => return None
		}
		Some(symbolTable)
	} catch { case e: Throwable => None }
	else None
	

	//try to match all 'from' rules, if success: produce a 'to' rule
	def applyRule(st: SymbolTable, rules: TransformerRule, grammar: Grammar): Option[(List[GrammarRule], SymbolTable, List[GrammarRule])] = {
		var symbolTable = st
		var matchedRules = List[GrammarRule]()
		
		for(rule <- rules.from){
			var matched = false
			for(grammarRule <- grammar.rules){
				matches(symbolTable, grammarRule, rule) match {
					case Some(nst) => {
						symbolTable = nst
						matched = true
						matchedRules = grammarRule :: matchedRules
					}
					case None => {}
				}
			}
			//if we couldn't match the 'from'-rule with any grammar rule, return None
			if(!matched) return None
		}
		
		//produce the out rules
		var out = List[GrammarRule]()
		for(rule <- rules.to){
			val (nst, r) = makeOutRule(symbolTable, rule)
			symbolTable = nst
			out = r :: out
		}
		//return a produced rule and the updated SymbolTable
		Some(matchedRules, symbolTable, out)
		
	}
	
	//make a new rule by applying the TransformerAtoms one by one, steadily updating the SymbolTable in case a new Nonterminal is introduced
	def makeOutRule(st: SymbolTable, prod: GrammarRuleMatcher): (SymbolTable, GrammarRule) = {
		var (st2, lhs) = applyMatcher(st, prod.lhs)
		var rhs = List[GrammarAtom]()
		for(atom <- prod.rhs){
			val (st3, at) = applyMatcher(st2, atom)
			//prepend for speed, then reverse at the end
			rhs = at +: rhs
			st2 = st3
		}
		rhs = rhs.reverse
		
		(st2, GrammarRule(lhs.asInstanceOf[Nonterminal], rhs, -1))
	}
	
	//produce a GrammarAtom from a TransformerAtom by looking up in the SymbolTable and adding new IDs
	def applyMatcher(st: SymbolTable, a: TransformerAtom): (SymbolTable, GrammarAtom) ={
		 a match {
			case NonterminalMatcher(id, _) => {
				val st2 = extendSymbolTable(st, id)
				(st2, Nonterminal(st2(id)))
			}
			case TerminalMatcher(str, _) => (st, Terminal(str))
			case IntegerMatcher(_) => (st, IntegerTerminal)
		}
	}
	
	//insert a new Symbol with the target ID
	def extendSymbolTable(st: SymbolTable, id: Int): SymbolTable = {
		if(st.isDefinedAt(id)) return st
		var next = "A"
		while(st.values.toList.contains(Symbol(next))){
			next = nextLexicographic(next)
		}
		st + ((id, Symbol(next)))
	}
	
	//increment on Strings over {\epsilon, A, B, C, ..., Z}
	def nextLexicographic(nts: String): String = {
		// \epsilon+1 == "A"
		if(nts.length == 0) "A"
		else {
			var res = ""
			var incrementing = true
			for(c <- nts.reverseIterator){
				//carry
				if(incrementing && c == 'Z') {
					res = 'A' + res
				}
				//increment
				else if(incrementing){
					res = ((c + 1).asInstanceOf[Char]) + res
					incrementing = false
				}
				//copy
				else{
					res = c + res
				}
			}
			res
		}
	}
	
	//to transform a Grammar, we take every transformRule and apply it, meanwhile updating the SymbolTable
	def transformGrammar(transformer: GrammarTransformer)(grammar: Grammar): (Grammar, PatternSynonyms) = {
		var outRules = List[GrammarRule]()
		var patternSynonyms = List[PatternSynonym]()
		var symbolTable: SymbolTable = Map()
		var i = 1
		for(rule <- transformer.rules) {
			applyRule(symbolTable, rule, grammar) match {
				//if we were able to apply the rule, update our SymbolTable
				case Some((matched, nst, grammarRules)) => { 
					symbolTable = nst
					val (newRules, i1) = tagGrammarRules(grammarRules, i)
					i = i1
					outRules = newRules ++ outRules
					var patterns = producePatternSynonyms(rule, matched, newRules, symbolTable)
					patternSynonyms = patternSynonyms ++ patterns
				}
				//we couldn't apply the rule, so we do nothing
				case None => {}
			}	
		}
		(Grammar(Nonterminal(symbolTable(transformer.start.id)), outRules), patternSynonyms)
	}
	
	def tagGrammarRules(rules: List[GrammarRule], nextTag: Int): (List[GrammarRule], Int) = rules match {
		case (GrammarRule(l, r, _)) :: rules2 => {
			val (l1, i) = tagGrammarRules(rules2, nextTag + 1)
			((GrammarRule(l, r, nextTag)) :: l1, i)
		}
		case Nil => (List[GrammarRule](), nextTag)
	}
	
	def concreteToAbstract = GrammarTransformer(NonterminalMatcher(0, 0), List(transformFtoA, transformCtoA, transformStoA))	
	
	
	sealed trait SyntaxTree
	case class Branch(nt: Symbol, childs: List[SyntaxTree]) extends SyntaxTree
	trait Leaf extends SyntaxTree
	case class LeafString(str: String) extends Leaf
	case class LeafInteger(i: Int) extends Leaf
	
	
		
	case class PatternSynonym(lhs: TypedPattern, rhs: TypedPattern){
		override def toString = lhs.toString + " = " + rhs.toString
	}
	type PatternSynonyms = List[PatternSynonym]
	sealed trait PatternAtom
	case class TypedPatternVariable(id: Int, typ: Symbol) extends PatternAtom{
		override def toString = "(" + id + " :: " + typ + ")"
	}
	case class PatternTerminal(str: String) extends PatternAtom{
		override def toString = " " + str + " "
	}
	case class PatternInteger(id: Int) extends PatternAtom{
		override def toString = "(int :: " + id + ")"
	}
	case class TypedPattern(ruleName: Int, patternContent: List[PatternAtom], typ: Symbol){
		override def toString = ruleName + ": " + patternContent.fold("")((a, b) => a.toString + b.toString) + " :: " + typ
	}
	
	//sealed abstract class TransformerAtom{def tag: Int}
	//case class NonterminalMatcher(id: Int, tag: Int) extends TransformerAtom
	//case class TerminalMatcher(matches: String, tag: Int) extends TransformerAtom
	//case class IntegerMatcher(tag: Int) extends TransformerAtom
	//
	//type TransformerSequence = List[TransformerAtom] 
	//case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence)
	//case class TransformerRule(from: List[GrammarRuleMatcher], to: List[GrammarRuleMatcher])
	//type TransformerRules = List[TransformerRule]
	//case class GrammarTransformer(start: NonterminalMatcher, rules: TransformerRules)
	
	
	
	def producePatternSynonyms(tRule: TransformerRule, matchedRules: GrammarRules, producedRules: GrammarRules, symbolTable: SymbolTable): PatternSynonyms = {
		println("PROD-PAT-SYN:")
		tRule.from.foreach(println)
		tRule.to.foreach(println)
		println
		matchedRules.foreach(println)
		println
		producedRules.foreach(println)
		println
		println(symbolTable)
		println("END")
		println
		
		for(fromRule <- tRule.from; toRule <- tRule.to; if(matching(fromRule, toRule))) yield {
			PatternSynonym(
				TypedPattern(-1, fromRule.rhs.map(translatePatternAtom(symbolTable)), symbolTable(fromRule.lhs.id)),
				TypedPattern(-2,   toRule.rhs.map(translatePatternAtom(symbolTable)), symbolTable(  toRule.lhs.id))	
			)
		}
	}
	
	def matching(from: GrammarRuleMatcher, to: GrammarRuleMatcher): Boolean = {
		for(toAtom <- to.rhs){
			println(toAtom)
			toAtom match {
				case _:NonterminalMatcher if(toAtom.tag != -1 && !(names(from)(toAtom))) => return false
				case _ => {}
			} 
		}
		return true
	}
	
	def names(from: GrammarRuleMatcher)(toAtom: TransformerAtom): Boolean = {
		println
		from.rhs.foreach(println)
		for(fromAtom <- from.rhs) fromAtom match {
			case NonterminalMatcher(_, tagFrom) if(tagFrom == toAtom.tag) => return true
			case _ => {}
		}
		println("not matched: " + toAtom)
		return false
	}
	
	def translatePatternAtom(symbolTable: SymbolTable)(x: TransformerAtom): PatternAtom =  x match {
			case NonterminalMatcher(id, tag) => TypedPatternVariable(tag, symbolTable(id))
			case TerminalMatcher(m, tag) => PatternTerminal(m)
			case IntegerMatcher(tag) => PatternInteger(tag)
		}
	
	
	def main(args: Array[String]) = {
		println(g1)
		val (newG, ps) = transformGrammar(concreteToAbstract)(g1)
		ps.foreach(println)
		//println((parseWithGrammar(g1)("5+6*6")).treeString)
	}
	
	
	//--------- P A R S I N G -----------
	
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
  
  def listToOption[A](l: List[Parser[A]]): Parser[A] = l.reduce((x, y) => x | y)
  	
	def parseString(expected: String): Parser[String] = code => {
    if (code startsWith expected) Some((expected, code drop expected.length))
    else None
  }
  def parseRegex(regex: String): Parser[String] = code => {
    val Pattern = s"($regex)(.*)".r
    code match {
      case Pattern(groups @ _*) => {
      	Some((groups.head, groups.last))
      }
      case otherwise => {
      	None
      }
    }
  }
  def keywordParser(keyword: String): Parser[SyntaxTree] = 
    parseString(keyword) ^^ { x => LeafString(keyword) }
  
  def digitsParser: Parser[SyntaxTree] = 
    parseRegex("[0-9]+") ^^ { x => LeafInteger(x.toInt) }
  
  
	def parseNonterminal(nonterminal: Nonterminal, grammar: Grammar): Parser[SyntaxTree] = {
    listToOption((grammar lookup nonterminal).map(parseRHS(_, grammar) ^^ {
      children => Branch(nonterminal.sym, children)
    }))
  }
	
	def parseRHS(ruleRHS: List[GrammarAtom], grammar: Grammar): Parser[List[SyntaxTree]] = ruleRHS match {
		case head :: tail => recurseParseRHS(head, tail, grammar)
		case Nil => {s => Some((Nil, s))}
	}
	
	def recurseParseRHS(head: GrammarAtom, tail: List[GrammarAtom], grammar: Grammar): Parser[List[SyntaxTree]] = { 
		parseGrammarAtom(head, grammar) ~ parseRHS(tail, grammar) ^^ {
			case (t1, t2) => t1 :: t2
		}
	}
	
	def parseGrammarAtom(head: GrammarAtom, grammar: Grammar): Parser[SyntaxTree] = head match {
				case nt: Nonterminal => parseNonterminal(nt, grammar)
				case Terminal(str) => keywordParser(str)
				case IntegerTerminal => digitsParser
			}
			
	def parseWithGrammar(g: Grammar)(str: String): SyntaxTree = {
		var parser = parseNonterminal(g.start, g)
    parser(str) match {
      case Some((t, rest)) if (rest == "") => t
      case None => sys.error("Not an Expression: " + str)

    	}
  	}
}