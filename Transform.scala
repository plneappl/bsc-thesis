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
	
	val leftBrace  = Terminal("(")
	val rightBrace = Terminal(")")
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
	case object RightArrow extends TransformerAtom
	case class TransformerOr(either: TransformerSequence, or: TransformerSequence) extends TransformerAtom
	
	case class TransformerSequence(lhs: NonterminalMatcher, rhs: List[TransformerAtom])
	case class TransformerRule(from: TransformerSequence, to: TransformerSequence)
	type TransformerRules = List[TransformerRule]
	
	
	def main(args: Array[String]) = {
		println(g1)
		println(g2)
	}
}