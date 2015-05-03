object Transform {
	trait GrammarAtom
	case class Nonterminal(sym: Symbol) extends GrammarAtom
	case class Terminal(sym: String) extends GrammarAtom
	case object IntegerTerminal extends GrammarAtom
	
	case class GrammarRule(lhs: Nonterminal, rhs: List[GrammarAtom])
	
	case class Grammar(start: Nonterminal, rules: List[GrammarRule])
	
	val c = Nonterminal('C)
	val a = Nonterminal('A)
	val s = Nonterminal('S)
	val f = Nonterminal('F)
	
	val leftBrace  = Terminal("[")
	val rightBrace = Terminal("]")
	val plus       = Terminal("+")
	val mul			   = Terminal("*")
	
	val rules1 = List(
		GrammarRule(c, List(c, plus, s)),
		GrammarRule(c, List(s)),
		GrammarRule(s, List(s, mul, f)),
		GrammarRule(s, List(f)),
		GrammarRule(f, List(IntegerTerminal)),
		GrammarRule(f, List(leftBrace, c, rightBrace))
		)
	
	val g1 = Grammar(c, rules1)
	
	val rules2 = List(
		GrammarRule(a, List(a, plus, a)),
		GrammarRule(a, List(a, mul, a)),
		GrammarRule(a, List(IntegerTerminal))
		)
	val g2 = Grammar(a, rules2)
	
	trait TransformerAtom
	case class NonterminalMatcher(id: Int) extends TransformerAtom
	case class TerminalMatcher(matches: String) extends TransformerAtom
	case object IntegerMatcher extends TransformerAtom
	
	type TransformerSequence = List[TransformerAtom] 
	case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence)
	case class TransformerRule(from: List[GrammarRuleMatcher], to: GrammarRuleMatcher)
	type TransformerRules = List[TransformerRule]
	
	val transformCtoA = TransformerRule(
			List(
				GrammarRuleMatcher(NonterminalMatcher(1), List(NonterminalMatcher(1), TerminalMatcher("+"), NonterminalMatcher(2))),
				GrammarRuleMatcher(NonterminalMatcher(1), List(NonterminalMatcher(2)))
			),
			GrammarRuleMatcher(NonterminalMatcher(0), List(NonterminalMatcher(0), TerminalMatcher("+"), NonterminalMatcher(0)))
		)
	
	val transformStoA = TransformerRule(
			List(
				GrammarRuleMatcher(NonterminalMatcher(2), List(NonterminalMatcher(2), TerminalMatcher("*"), NonterminalMatcher(3))),
				GrammarRuleMatcher(NonterminalMatcher(2), List(NonterminalMatcher(3)))
			),
			GrammarRuleMatcher(NonterminalMatcher(0), List(NonterminalMatcher(0), TerminalMatcher("*"), NonterminalMatcher(0)))
		)
	
	val transformFtoA = TransformerRule(
			List(
				GrammarRuleMatcher(NonterminalMatcher(3), List(TerminalMatcher("["), NonterminalMatcher(1), TerminalMatcher("]"))),
				GrammarRuleMatcher(NonterminalMatcher(3), List(IntegerMatcher))	
			),
			GrammarRuleMatcher(NonterminalMatcher(0), List(NonterminalMatcher(0)))
		)
	
	type SymbolTable = Map[Int, Symbol]
	
	def checkSymbolTable(st: SymbolTable, sym: Symbol, id: Int): SymbolTable = st.get(id) match {
		case Some(sym2) if(sym2 == sym) => st
		case None => st + ((id, sym))
	}
		
	def matches(st: SymbolTable, grammarRule: GrammarRule, matcher: GrammarRuleMatcher): Option[SymbolTable] = if(grammarRule.rhs.size == matcher.rhs.size) try {
		var symbolTable = checkSymbolTable(st, grammarRule.lhs.sym, matcher.lhs.id)
		for((ga, ma) <- (grammarRule.rhs zip matcher.rhs)) (ga, ma) match {
			case (Nonterminal(sym), NonterminalMatcher(id)) => symbolTable = checkSymbolTable(st, sym, id)
			case (Terminal(str1), TerminalMatcher(str2)) if (str1 == str2)	=> {}
			case (IntegerTerminal, IntegerMatcher) => {}
			case _ => return None
		}
		Some(symbolTable)
	} catch { case e: Throwable => None }
	else None
	

	
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
			if(!matched) return None
		}
		
		Some(makeOutRule(symbolTable, rules.to))
	}
	
	def makeOutRule(st: SymbolTable, prod: GrammarRuleMatcher): (SymbolTable, GrammarRule) = {
		var (st2, lhs) = applyMatcher(st, prod.lhs)
		var rhs = List[GrammarAtom]()
		for(atom <- prod.rhs){
			val (st3, at) = applyMatcher(st2, atom)
			rhs = at +: rhs
			st2 = st3
		}
		rhs = rhs.reverse
		(st2, GrammarRule(lhs.asInstanceOf[Nonterminal], rhs))
	}
	
	def applyMatcher(st: SymbolTable, a: TransformerAtom): (SymbolTable, GrammarAtom) ={
		 a match {
			case NonterminalMatcher(id) => {
				val st2 = extendSymbolTable(st, id)
				(st2, Nonterminal(st2(id)))
			}
			case TerminalMatcher(str) => (st, Terminal(str))
		}
	}
	
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
	
	def transformGrammar(transformerRules: TransformerRules)(grammar: Grammar): Grammar = {
		var outRules = List[GrammarRule]()
		var symbolTable: SymbolTable = Map()
		for(rule <- transformerRules) {
			applyRule(symbolTable, rule, grammar) match {
				case Some((nst, newRule)) => { 
					outRules = newRule :: outRules 
					symbolTable = nst
				}
				case None => {}
			}	
		}
		Grammar(Nonterminal('A), outRules)
	}
	
	def main(args: Array[String]) = {
		println(g1)
		println(transformGrammar(List(transformCtoA, transformFtoA, transformStoA))(g1))
	}
}