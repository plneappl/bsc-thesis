object Test{
	import Transform._
	import sext._
	import Grammar._
	import ReadableSyntaxGrammar.{grammar => grammarSyntaxDef, _}
	
	val notLeftFactored: Grammar = Grammar(
		Nonterminal('A),
		List(
			GrammarRule(Nonterminal('A), List(Nonterminal('B), Nonterminal('A)), 1),
			GrammarRule(Nonterminal('A), List(Nonterminal('B), Nonterminal('D)), 2),
			GrammarRule(Nonterminal('D), List(Terminal("d")), 3),
			GrammarRule(Nonterminal('B), List(Terminal("b")), 4)
		))
	
	def main(args: Array[String]) = {
		testTransformer(g1)("concreteToAbstract.tr")

		//testTransformer(g1)("chomsky1.tr")
		//testTransformer(g1)("chomsky2.tr")

		//testTransformer(g1)("inlining.tr")
		
		//testTransformer(notLeftFactored)("leftFactoring.tr")
	}
	
	def testTransformer(g: Grammar)(file: String) = {
		val tr = getGrammarTransformer(file)
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

	def main1(args: Array[String]) = {
		println(g1)
		println(concreteToAbstract)
		val (newG, ps) = transformGrammar(concreteToAbstract)(g1)
		println
		println(grammarSyntaxDef)
		ps.foreach(println)
		println(ps.treeString)
		//println((parseWithGrammar(g1)("5+6*6")).treeString)
		val tr3 = getGrammarTransformer("readableSyntax1.tr")
		
		println(tr3)
		println(tr3.rules)
	}
	
	def main2(args: Array[String]) = {
		val ch1 = getGrammarTransformer("chomsky1.tr")
		val ch2 = getGrammarTransformer("chomsky2.tr")
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
		val tr3 = getGrammarTransformer("readableSyntax1.tr")
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