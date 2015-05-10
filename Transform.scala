object Transform {
	
	trait GrammarAtom
	case class Nonterminal(sym: Symbol) extends GrammarAtom
	case class Terminal(sym: String) extends GrammarAtom
	case object IntegerTerminal extends GrammarAtom
	
	//todo: 
	/*
	- produce patterns (?)
	*/
	case class GrammarRule(lhs: Nonterminal, rhs: List[GrammarAtom], tag: Int)
	
	case class Grammar(start: Nonterminal, rules: List[GrammarRule]){
		def lookup(nonterminal: Nonterminal): List[List[GrammarAtom]] = rules.filter({gr => gr.lhs.sym == nonterminal.sym}).map(r => r.rhs)
	}
	
	val c = Nonterminal('C)
	val a = Nonterminal('A)
	val s = Nonterminal('S)
	val f = Nonterminal('F)
	
	val leftBrace  = Terminal("[")
	val rightBrace = Terminal("]")
	val plus       = Terminal("+")
	val mul			   = Terminal("*")
	
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
	
	abstract class TransformerAtom{def tag: Int}
	case class NonterminalMatcher(id: Int, tag: Int) extends TransformerAtom
	case class TerminalMatcher(matches: String, tag: Int) extends TransformerAtom
	case class IntegerMatcher(tag: Int) extends TransformerAtom
	
	type TransformerSequence = List[TransformerAtom] 
	case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence)
	case class TransformerRule(from: List[GrammarRuleMatcher], to: GrammarRuleMatcher)
	type TransformerRules = List[TransformerRule]
	case class GrammarTransformer(start: NonterminalMatcher, rules: TransformerRules)
	
	//C to A: take C -> C + S, C -> S and produce A -> A + A
	val transformCtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(1, 0), List(NonterminalMatcher(1, 1), TerminalMatcher("+", 2), NonterminalMatcher(2, 3))),
			GrammarRuleMatcher(NonterminalMatcher(1, 0), List(NonterminalMatcher(2, -1)))
		),
		GrammarRuleMatcher(NonterminalMatcher(0, 0), List(NonterminalMatcher(0, 1), TerminalMatcher("+", 2), NonterminalMatcher(0, 3)))
	)
	
	//S to A: take S -> S * F, S -> F and produce A -> A * A
	//S0 -> S * F = A -> A * A
	//S1 -> F     = A -> A * A
	// 2 -> S, 3 -> F, 0 -> A
	//S -> A 
	val transformStoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(2, 0), List(NonterminalMatcher(2, 1), TerminalMatcher("*", 2), NonterminalMatcher(3, 3))),
			GrammarRuleMatcher(NonterminalMatcher(2, 0), List(NonterminalMatcher(3, -1)))
		),
		GrammarRuleMatcher(NonterminalMatcher(0, 0), List(NonterminalMatcher(0, 1), TerminalMatcher("*", 2), NonterminalMatcher(0, 3)))
	)

	
	//F to A: take F -> [ C ], F -> int and produce A -> int
	val transformFtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher(3, 0), List(TerminalMatcher("[", -1), NonterminalMatcher(1, -1), TerminalMatcher("]", -1))),
			GrammarRuleMatcher(NonterminalMatcher(3, 0), List(IntegerMatcher(1)))	
		),
		GrammarRuleMatcher(NonterminalMatcher(0, 0), List(IntegerMatcher(1)))
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
	def applyRule(st: SymbolTable, rules: TransformerRule, grammar: Grammar): Option[(SymbolTable, GrammarRule)] = {
		var symbolTable = st

		for(rule <- rules.from){
			var matched = false
			for(grammarRule <- grammar.rules){
				matches(symbolTable, grammarRule, rule) match {
					case Some(nst) => {
						symbolTable = nst
						matched = true
					}
					case None => {}
				}
			}
			//if we couldn't match the 'from'-rule with any grammar rule, return None
			if(!matched) return None
		}
		
		//return a produced rule and the updated SymbolTable
		Some(makeOutRule(symbolTable, rules.to))
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
	def transformGrammar(transformer: GrammarTransformer)(grammar: Grammar): Grammar = {
		var outRules = List[GrammarRule]()
		var symbolTable: SymbolTable = Map()
		var i = 1
		for(rule <- transformer.rules) {
			applyRule(symbolTable, rule, grammar) match {
				//if we were able to apply the rule, update our SymbolTable
				case Some((nst, GrammarRule(lhs, rhs, _))) => { 
					outRules = (GrammarRule(lhs, rhs, i)) :: outRules
					i = i + 1 
					symbolTable = nst
				}
				//we couldn't apply the rule, so we do nothing
				case None => {}
			}	
		}
		//How do we get the starting Nonterminal?
		Grammar(Nonterminal(symbolTable(transformer.start.id)), outRules)
	}
	
	def concreteToAbstract = GrammarTransformer(NonterminalMatcher(0, 0), List(transformFtoA, transformCtoA, transformStoA))	
	
	
	trait SyntaxTree
	case class Branch(nt: Symbol, childs: List[SyntaxTree]) extends SyntaxTree
	trait Leaf extends SyntaxTree
	case class LeafString(str: String) extends Leaf
	case class LeafInteger(i: Int) extends Leaf
	
	
		
	def main(args: Array[String]) = {
		println(g1)
		println(transformGrammar(concreteToAbstract)(g1))
	}
}