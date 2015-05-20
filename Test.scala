object Test{
	import Transform._
	import sext._
	import Grammar._
	import ReadableSyntaxGrammar.{grammar => grammarSyntaxDef, docToTransformerRules}
	
	
	def main(args: Array[String]) = {
		println(g1)
		println(concreteToAbstract)
		val (newG, ps) = transformGrammar(concreteToAbstract)(g1)
		println
		println(grammarSyntaxDef)
		ps.foreach(println)
		println(ps.treeString)
		println((parseWithGrammar(g1)("5+6*6")).treeString)
		val source = scala.io.Source.fromFile("readableSyntax1.tr")
		val tr = source.mkString
		
		source.close
		val tr2 = (parseWithGrammar(grammarSyntaxDef)(tr))
		val tr3 = docToTransformerRules(tr2)
		println(tr3)
	}
	
}