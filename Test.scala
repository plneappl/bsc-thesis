object Test{
	import Transform._
	import sext._
	import Grammar._
	import ReadableSyntaxGrammar.{grammar => grammarSyntaxDef, _}
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
	val rulesLR = List(
		GrammarRule(s, List(s, plus, f), 1),
		GrammarRule(s, List(f), 2),
		GrammarRule(f, List(IntegerTerminal), 3)
		)
	
	val g1 = Grammar(c, rules1)
	val gLR = Grammar(s, rulesLR)
	
	//abstract grammar
	val rules2 = List(
		GrammarRule(a, List(a, plus, a), 1),
		GrammarRule(a, List(a, mul, a), 2),
		GrammarRule(a, List(IntegerTerminal), 3)
		)
	val g2 = Grammar(a, rules2)
		

	val notLeftFactored: Grammar = Grammar(
		a,
		List(
			GrammarRule(a, List(Nonterminal('B), a), 1),
			GrammarRule(a, List(Nonterminal('B), Nonterminal('D)), 2),
			GrammarRule(Nonterminal('D), List(Terminal("d")), 3),
			GrammarRule(Nonterminal('B), List(Terminal("b")), 4)
		))
	
	def main(args: Array[String]) = {
		//testTransformer(gLR)("eliminateLeftRecursion.tr")
		//testTransformer(g1)("concreteToAbstract.tr")
		//testTransformer(g1)("concreteToAbstract2.tr")

		testTransformer(g1)("chomsky1.tr")
		//testTransformer(g1)("chomsky2.tr")

		//testTransformer(g1)("inlining.tr")
		
		//testTransformer(notLeftFactored)("leftFactoring.tr")
	}
	
	def testTransformer(g: Grammar)(file: String) = {
		val tr = getGrammarTransformers(file).head
		val (newG, ps) = transformGrammar(tr)(g)
		println("--------- " + file + " ---------")
		println("--------- Transforming: ---------")
		println(g)
		println("--------- Using: ---------")
		println(tr)
		println("--------- Result: ---------")
		println(newG)
		println("--------- Pattern Synonyms: ---------")
		ps.foreach(println)
		println("------------------")
	}

	
	def main2(args: Array[String]) = {
		val ch1 = getGrammarTransformers("chomsky1.tr").head
		val ch2 = getGrammarTransformers("chomsky2.tr").head
		val (newG, ps) = transformGrammar(ch1)(g1)
		println
		println(g1)
		println
		println(newG)
		val (newG2, ps2) = transformGrammar(ch2)(newG)
		println(newG2)
		println
		ps2.foreach(println)
	}
	
	def main3(args: Array[String]) = {
		val tr3 = getGrammarTransformers("readableSyntax1.tr").head
		val (newG, ps) = transformGrammar(tr3)(g1)
		println
		println(g1)
		println
		println(tr3)
		println
		//g1.rules.combinations(2).map(_.permutations).flatten.toList.foreach(println)
		println
		println(newG)
		println
		ps.foreach(println)
	}
}