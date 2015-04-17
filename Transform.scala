
import java.util.regex.Pattern

object Transform {
	//TransformationRules are 1. a Pattern matching a rule to be transformed 2. output patterns, that create rules.
	type TransformationRule = (String, List[String])
	//A Transformation Object is a regex for pattern matching, format strings for outputting new rules,
	//and the number of new Nonterminals beeing introduced.
	type TransformationObject = (String, List[String], Int)
	//TokenIndices is a Map mapping Nonterminals to their first appearance in a pattern
	//therefore "A -> BDC" will make a map like Map(A -> 1, B -> 2, D -> 3, C -> 4)
	type TokenIndices = Map[String, Int]
	//This is a rule to be transformed; we want to factor out the 'B'
	val in: String = "A -> BC | BD"
	//Transformation rules see everything separated by spaces as tokens. Nonterminals are anything starting with a capital letter.
	//New nonterminals may be introduced simply by using them, the next lexicographically available nt will be used.
	val factor: TransformationRule = ("N1 -> N2 N3 | N2 N4", List("N1 -> N2 N5", "N5 -> N3 | N4"))
	//Output shoud be: A -> B B'; B' -> C | D

	//buildTransformationRules takes a TransformationRule and a grammatical rule
	def buildTransformationRules(transformationRule: TransformationRule)(rule: String):TransformationObject = {
		//build the in-pattern-matchers
		var (inRegex, knownTokens) = buildInRegex(transformationRule._1)

		var knownTokensCount = knownTokens.size
		var result = List[String]()

		//build the out-patterns
		for(outRule <- transformationRule._2){
			val (outFString, knownTokens2) = buildOutFormatString(knownTokens)(outRule)
			//update known nonterminals
			knownTokens = knownTokens2
			result = result :+ outFString
		}
		//tokens introduced by the out-patterns
		var newTokensCount = knownTokens.size - knownTokensCount 
		(inRegex, result, newTokensCount)
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
	def applyTransformationRules(to: TransformationObject)(in: String): List[String] = {
		var result = List[String]()
		println()
		println("Pattern: " + to._1)
		println("Trying to match: " + in)

		var numberOfNewNonterminals = to._3

		//this is the "complicated" part: finding out how to get a list of matches
		var vars = to._1.r.findFirstMatchIn(in).get.subgroups

		//add new nonterminals, starting at "A"
		var nextNonterminal = "A"
		for(i <- 0 until numberOfNewNonterminals){
			//just try the next lexicographical one until it's a new one
			while(vars.contains(nextNonterminal)) {
				nextNonterminal = nextLexicographic(nextNonterminal)
			}
			vars = vars :+ nextNonterminal 
		}
		
		println("Vars: " + vars)	

		to._2.foreach(println)
		to._2.foreach(x => {
			//apply the format strings to get the transformed rules
			result = x.format(vars:_*) :: result
		})
		result
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
						result += "([A-Z])\\s*"
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

	//wrapper for easier use
	def transformGrammar(factor: TransformationRule)(in: String) = {
		var to = buildTransformationRules(factor)(in)
		applyTransformationRules(to)(in)
	}

	def main(args: Array[String]): Unit = {
		var transformed = transformGrammar(factor)(in)
		println()
		println("In: ")
		println(in)
		println()
		println("Transformed: ")
		transformed.foreach(println)
		println()
	}
}