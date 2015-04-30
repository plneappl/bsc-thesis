import java.util.regex.Pattern

object Transform {
	
	//TransformationRules are 1. a Pattern matching a rule to be transformed 2. output patterns, that create rules.
	case class TransformationRule(fromPattern: String, toPatterns: List[String])
	//A Transformation Object is a regex for pattern matching, format strings for outputting new rules,
	//and the number of new Nonterminals beeing introduced.
	case class TransformationObject(fromRegex: String, toFStrings: List[String], newNTsCount: Int)
	//TokenIndices is a Map mapping Nonterminals to their first appearance in a pattern
	//therefore "A -> BDC" will make a map like Map(A -> 1, B -> 2, D -> 3, C -> 4)
	type TokenIndices = Map[String, Int]
	//This is a rule to be transformed; we want to factor out the 'B'
	val in: String = "A -> BC | BD"
	
	//Transformation rules see everything separated by spaces as tokens. Nonterminals are anything starting with a capital letter.
	//New nonterminals may be introduced simply by using them, the next lexicographically available nt will be used.
	val factor = TransformationRule("N1 -> N2 N3 | N2 N4", List("N1 -> N2 N5", "N5 -> N3 | N4"))
	//Output shoud be: A -> B B'; B' -> C | D

	//buildTransformationRules takes a TransformationRule and a grammatical rule
	def buildTransformationRule(transformationRule: TransformationRule)(rule: String):TransformationObject = {
		//build the in-pattern-matchers
		var (inRegex, knownTokens) = buildInRegex(transformationRule.fromPattern)

		var knownTokensCount = knownTokens.size
		var result = List[String]()

		//build the out-patterns
		for(toPattern <- transformationRule.toPatterns){
			val (outFString, knownTokens2) = buildOutFormatString(knownTokens)(toPattern)
			//update known nonterminals
			knownTokens = knownTokens2
			result = result :+ outFString
		}
		//tokens introduced by the out-patterns
		var newTokensCount = knownTokens.size - knownTokensCount 
		TransformationObject(inRegex, result, newTokensCount)
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

	//transform a grammar rule
	def applyTransformationRule(tfo: TransformationObject)(in: String): (List[String], List[FlowFragment]) = {
		var result = List[String]()
		var res2   = List[FlowFragment]()
		println()
		println("Pattern: " + tfo.fromRegex)
		println("Trying to match: " + in)

		//this is the "complicated" part: finding out how to get a list of matches
		var vars = tfo.fromRegex.r.findFirstMatchIn(in).get.subgroups

		//add new nonterminals, starting at "A"
		var nextNonterminal = "A"
		for(i <- 0 until tfo.newNTsCount){
			//just try the next lexicographical one until it's a new one
			while(vars.contains(nextNonterminal)) {
				nextNonterminal = nextLexicographic(nextNonterminal)
			}
			vars = vars :+ nextNonterminal 
		}
		
		println("Vars: " + vars)	

		var i = 1
		tfo.toFStrings.foreach(println)
		tfo.toFStrings.foreach(x => {
			//apply the format strings to get the transformed rules
			val newRule = x.format(vars:_*)
			result = result :+ newRule 
			
			i = i + 1
		})
		println(res2)
		(result, List(
			FlowFragment(ProductionRuleTransform(ProductionRuleIdentifier("A", 1), ProductionRuleIdentifier("A", 1)), List(Rename(("B", 1), ("B", 1)), Create(("C", 2), (("E", 2, 2), ("C", 3))))),
			FlowFragment(ProductionRuleTransform(ProductionRuleIdentifier("A", 2), ProductionRuleIdentifier("A", 1)), List(Rename(("B", 3), ("B", 1)), Create(("D", 4), (("E", 3, 2), ("D", 4)))))
			)
		)
	}

	//taking a out-rule and a map of known nonterminals, build an out format string
	def buildOutFormatString(tokIndices: TokenIndices)(rule: String): (String, TokenIndices) = {
		//Tokens are separated by spaces
		val tokens = rule.split(" ")
		var result = ""
		//Nonterminals start with a capital letter
		val nonTerminal = "[A-Z].*".r
		//read/write
		var known = tokIndices
		var ntCount = known.size + 1
		for(tok <- tokens) {
			tok match {
				//replace nonterminals by the index in the ntMap, add them if unknown
				case nonTerminal(_*) => {
					if(known.getOrElse(tok, 0) == 0) {
						//new nonterminal, use it and return in the Map
						known += (tok -> ntCount)
						ntCount += 1
					}
					
					result += "%" + known(tok) + "$s"
					
				}
				//terminals are not transformed
				case _ => {
					result += tok
				}
			}
		}
		//return the map, so other format strings know about new nonterminals
		(result, known)
	}

	//this works quite the same as buildOutFormatString, differences are commented
	//also adds arbitrary space between tokens, just to be safe
	def buildInRegex(rule: String): (String, TokenIndices) = {
		val tokens = rule.split(" ")
		var known = Map[String, Int]()
		var result = ""
		val nonTerminal = "[A-Z].*".r
		var subExprCount = known.size + 1
		for(tok <- tokens) {
			tok match {
				case nonTerminal(_*) => {
					if(known.getOrElse(tok, 0) == 0) {
						
						known += (tok -> subExprCount)
						subExprCount += 1
						
						//difference: first occurences of nonterminals are used to parse them, later ones will get backreferenced
						result += "([A-Z][a-z]*)\\s*"
					}
					else {
						//backreference 
						result +=  "\\" + known(tok)
					}
				}
				case _ => {
					//quote every non-nonterminal
					result += Pattern.quote(tok) + "\\s*"
				}
			}	
		}
		(result, known)
	}

	trait InformationPath
	case class Rename(from:(String, Int), to:(String, Int)) extends InformationPath
	case class Create(from:(String, Int), to:((String, Int, Int), (String, Int))) extends InformationPath
	case class Delete(from:(String, Int), to:String) extends InformationPath
	case class ProductionRuleIdentifier(nonterminal: String, index: Int)
	case class ProductionRuleTransform(from: ProductionRuleIdentifier, to: ProductionRuleIdentifier)
	case class FlowFragment(transform: ProductionRuleTransform, informationPaths: List[InformationPath])
	type InformationFlow = List[FlowFragment]

	//wrapper for easier use
	def transformGrammarRule(factor: TransformationRule)(in: String): (List[String], InformationFlow) = {
		var to = buildTransformationRule(factor)(in)
		applyTransformationRule(to)(in)
		
	}
	

	def main(args: Array[String]): Unit = {
		var transformed = transformGrammarRule(factor)(in)
		println()
		println("In: ")
		println(in)
		println()
		println("Transformed: ")
		println(transformed)
	}
}