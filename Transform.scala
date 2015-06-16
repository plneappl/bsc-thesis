object Transform {
	
	import Grammar._
	import sext._
	import collection.mutable.{ ListBuffer, HashMap, MultiMap, Set => MSet }
	
	
	sealed abstract class TransformerAtom{
		def tag: String
		def copy(t: String): TransformerAtom
	}
	case class NonterminalMatcher(name: String, tag: String) extends TransformerAtom {
		override def toString = name +  (if(tag != "") ":" + tag else "")
		def copy(t: String) = NonterminalMatcher(name, t)
	}
	case class TerminalMatcher(name: String, tag: String) extends TransformerAtom {
		override def toString = if(tag != "") name + ":" + tag else name
		def copy(t: String) = TerminalMatcher(name, t)
	}
	case class LiteralMatcher(matches: String, tag: String) extends TransformerAtom {
		override def toString = if(tag != "") s""""$matches":$tag""" else s""""$matches""""               //"//again, for syntax highlighting
		def copy(t: String) = LiteralMatcher(matches, t)
	}
	case class IntegerMatcher(tag: String) extends TransformerAtom {
		override def toString = "<int>:" + tag
		def copy(t: String) = IntegerMatcher(t)
	}
	
	type TransformerSequence = List[TransformerAtom] 
	type TransformerRules = List[TransformerRule]
	
	case class GrammarRuleMatcher(lhs: NonterminalMatcher, rhs: TransformerSequence) {
		override def toString = lhs + " ->" + rhs.map(_.toString).fold("")(joinStringsBy(" "))
	}
	case class TransformerRule(from: List[GrammarRuleMatcher], to: List[GrammarRuleMatcher]) {
		def asString(indent: String) = indent + "from:" + 
			from.map(_.toString).fold("")(joinStringsBy("\n  " + indent)) +
			"\n" + indent + "to:" +
			to.map(_.toString).fold("")(joinStringsBy("\n  " + indent))
	}
	
	case class GrammarTransformer(start: NonterminalMatcher, rules: TransformerRules) {
		override def toString = "GrammarTransformer:" +
			"\n  Start: " + start +
			"\n  Rules:"  + rules.map(_.asString("    ")).fold("")(joinStringsBy("\n")) + 
			"\n"
	}
	
	
	
	//NonterminalMatchers can identify themselves with any symbol, this stores the identification while transforming
	type SymbolTable = Map[String, Symbol]
	type UsedSymbols = Set[Symbol]
	
	//insert unknown symbols and check known for equality, fail if not equal
	def checkSymbolTable(st: SymbolTable, name: String, sym: Symbol): SymbolTable = {
		st.get(name) match {
			case Some(sym2) if(sym2 == sym) => st
			case None => st + ((name, sym))
		}}
		
	//match a grammar rule with a matcher one atom at a time	
	def matches(st: SymbolTable, grammarRule: GrammarRule, matcher: GrammarRuleMatcher): Option[SymbolTable] = if(grammarRule.rhs.size == matcher.rhs.size) try {
		var symbolTable = checkSymbolTable(st, matcher.lhs.name, grammarRule.lhs.sym)
		for((ga, ma) <- (grammarRule.rhs zip matcher.rhs)) {
			(ga, ma) match {
				case (Nonterminal(sym), NonterminalMatcher(id, _)) => symbolTable = checkSymbolTable(symbolTable, id, sym)
				case (Terminal(str1), TerminalMatcher(id, _)) => symbolTable = checkSymbolTable(symbolTable, id, Symbol(str1))
				case (Terminal(str1), LiteralMatcher(str2, _))	if (str1 == str2) => { }
				case (IntegerTerminal, IntegerMatcher(_)) => {}
				case x => {return None}
		}}
		Some(symbolTable)
	} catch { case e: Throwable => None }
	else None
	
	def lists[T1](n: Int)(l: List[T1]) = l.combinations(n).map(_.permutations).flatten
	def select[T1, T2](n: Int)(l: List[T1])(f: List[T1] => Option[T2]): List[T2] = lists(n)(l).map(f).flatten.toList

	def matchRulesToGrammarRules(from: List[GrammarRuleMatcher])(l: GrammarRules): Option[(SymbolTable, GrammarRules)] = {
			var symbolTable: SymbolTable = Map()
			var matchedRules = List[GrammarRule]()
			for((gRule, gMatcher) <- (l zip from)){
				matches(symbolTable, gRule, gMatcher) match {
					case Some(nst) => {
						symbolTable = nst
						matchedRules = gRule :: matchedRules
					}
					case None => return None
				}
			}
			Some((symbolTable, matchedRules))
		}

	type transformationInformation = (GrammarRules, SymbolTable, GrammarRules)

	//try to match all 'from' rules, on every success: produce 'to' rules
	def applyRule(transformerRules: TransformerRule, grammar: Grammar): Option[List[transformationInformation]] = {
		var selectN = select[GrammarRule, (SymbolTable, GrammarRules)](transformerRules.from.length)(grammar.rules) _
		var matchedRules = selectN(matchRulesToGrammarRules(transformerRules.from) _)
		
		//produce the out rules
		var out = List[(GrammarRules, SymbolTable, GrammarRules)]()
		
		var usedSymbols = Set[Symbol]()
		for((st, gRules) <- matchedRules){
			var symbolTable = st
			usedSymbols ++= st.values
			var newRules: GrammarRules = List()
			for(rule <- transformerRules.to){
				
				val (nst, nus, r) = makeOutRule(symbolTable, rule, usedSymbols)
				symbolTable = nst
				newRules = r :: newRules
				usedSymbols = nus
			}
			
			out = (gRules, symbolTable, newRules) :: out
			
			
		}
		//return produced rules and the SymbolTables
		Some(out)
	}
	
	//make a new rule by applying the TransformerAtoms one by one, steadily updating the SymbolTable in case a new Nonterminal is introduced
	def makeOutRule(st: SymbolTable, prod: GrammarRuleMatcher, usedSymbols: Set[Symbol]): (SymbolTable, UsedSymbols, GrammarRule) = {
		var (st2, us2, lhs) = applyMatcher(st, prod.lhs, usedSymbols)
		var rhs = List[GrammarAtom]()
		for(atom <- prod.rhs){
			val (st3, us3, at) = applyMatcher(st2, atom, us2)
			//prepend for speed, then reverse at the end
			rhs = at +: rhs
			st2 = st3
			us2 = us3
		}
		rhs = rhs.reverse
		
		(st2, us2, GrammarRule(lhs.asInstanceOf[Nonterminal], rhs, -1))
	}
	
	//produce a GrammarAtom from a TransformerAtom by looking up in the SymbolTable and adding new IDs
	def applyMatcher(st: SymbolTable, a: TransformerAtom, usedSymbols: Set[Symbol]): (SymbolTable, UsedSymbols, GrammarAtom) = {
		 a match {
			case NonterminalMatcher(id, _) => {
				val (st2, us2) = extendSymbolTable(st, id, usedSymbols)
				
				(st2, us2, Nonterminal(st2(id)))
			}
			case TerminalMatcher(id, _) => (st, usedSymbols, Terminal(st(id).name))

			case LiteralMatcher(str, _) => (st, usedSymbols, Terminal(str))
			case IntegerMatcher(_) => (st, usedSymbols, IntegerTerminal)
		}
	}
	
	//insert a new Symbol with the target ID
	def extendSymbolTable(st: SymbolTable, name: String, usedSymbols: Set[Symbol]): (SymbolTable, UsedSymbols) = {
		
		if(st.isDefinedAt(name)) return (st, usedSymbols)
		if(!st.values.toList.contains(Symbol(name)) && !usedSymbols.contains(Symbol(name))) return (st + ((name, Symbol(name))), usedSymbols + Symbol(name))
		var next = name
		while(st.values.toList.contains(Symbol(next)) || usedSymbols.contains(Symbol(next))){
			next = nextLexicographic(next)
		}
		(st + ((name, Symbol(next))), usedSymbols + Symbol(next))
	}
	
	def getNthVariableName(n: Int): String = {
		var cur = "A"
		for(i <- 0 until n){cur = nextLexicographic(cur)}
		cur
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
				else if(incrementing && c == 'z') {
					res = 'z' + res
				}
				else if(incrementing && c == '9') {
					res = '0' + res
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
			if(incrementing) res = 'A' + res
			res
		}
	}
	

	//to transform a Grammar, we take every transformRule and apply it, meanwhile updating the SymbolTable
	def transformGrammar(transformer: GrammarTransformer)(grammar: Grammar): (Grammar, PatternSynonyms) = {
		var outRules = List[GrammarRule]()
		var patternSynonyms = List[PatternSynonym]()
		var i = 1
		for(rule <- transformer.rules) {
			applyRule(rule, grammar) match {
				//if we were able to apply the rule, update our SymbolTable
				case Some(listOfResultPairs) => {
					for((matchedRules, symTable, producedRules) <- listOfResultPairs){ 
						val (producedRulesTagged, i1) = tagGrammarRules(producedRules, i)
						i = i1
						outRules = producedRulesTagged ++ outRules
						var patterns = producePatternSynonyms(rule, matchedRules, producedRulesTagged, symTable)
						patternSynonyms = patternSynonyms ++ patterns
					}
				}
				//we couldn't apply the rule, so we do nothing
				case None => { }
			}	
		}
		//appending ++ grammar.rules.filter(x => !x.matched) to outRules would be nice, but mixes up nonterminal names
		(Grammar(Nonterminal(Symbol(transformer.start.name)), outRules), patternSynonyms)
	}
	
	def tagGrammarRules(rules: List[GrammarRule], nextTag: Int): (List[GrammarRule], Int) = rules match {
		case (GrammarRule(l, r, _)) :: rules2 => {
			val (l1, i) = tagGrammarRules(rules2, nextTag + 1)
			((GrammarRule(l, r, nextTag)) :: l1, i)
		}
		case Nil => (List[GrammarRule](), nextTag)
	}
	
	
	
		
	case class PatternSynonym(lhs: TypedPattern, rhs: TypedPattern){
		override def toString = lhs.toString + " = " + rhs.toString
	}
	type PatternSynonyms = List[PatternSynonym]
	sealed trait PatternAtom{ def id: String; def getIds: Set[String] }
	case class TypedPatternVariable(id: String, typ: Symbol) extends PatternAtom{
		override def toString = "(" + id + " :: " + typ.name + ")"
		def getIds = Set(id)
	}
	case class PatternTerminal(id: String, str: String) extends PatternAtom{
		override def toString = "(" + id + " :: " + str + ")"
		def getIds = Set(id)
	}

	case class TypedPattern(patternName: Int, patternContent: List[PatternAtom], typ: Symbol) extends PatternAtom {
		def id = ""
		def getIds = patternContent.map(_.getIds).fold(Set())((x, y) => x ++ y)
		override def toString = "(" + typ.name + "_" + patternName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ.name + ")"
	}
	//Name as String
	//case class TypedPattern(ruleName: String, patternContent: List[PatternAtom], typ: Symbol){
	//	override def toString = ruleName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ
	//}
	
	type SeenTags = MultiMap[String, TransformerSequence]
	def newSeenTags():SeenTags = new HashMap[String, MSet[TransformerSequence]] with MultiMap[String, TransformerSequence]
	def addTags(rule: TransformerSequence)(to: SeenTags): SeenTags = {		
		var res = to
		rule.map(_.tag).filter(_ != "").foreach(x => res.addBinding(x, rule))
		res
	}	
	
	
	def pairs(max1: Int, max2: Int) = for(i <- (1 to (max1 * max2)).view; j1 <- (1 to max1).view; if(i - j1 >= 1 && i - j1 <= max2)) yield (j1, i - j1)
	
	def getTags(p: TypedPattern): Set[String] = {
		p.getIds.filter(_ != "").toSet[String]
	}
	
	//produce all PatternSynonyms where all variables on the right side get resolved by the left side
	//get the PatternSynonym name by looking up, which grammar rule matches the transformerRule
	
	def producePatternSynonyms(tRule: TransformerRule, matchedRules: GrammarRules, producedRules: GrammarRules, symbolTable: SymbolTable): PatternSynonyms = {
		var res = Set[PatternSynonym]()
		for((i, j) <- pairs(tRule.from.size, tRule.to.size); 
				fromRules <- lists(i)(tRule.from);
				toRules   <- lists(j)(tRule.to)) {
			
			//if(((needed.keySet &~ found.keySet) union (found.keySet &~ needed.keySet)).isEmpty && !found.isEmpty) {
				
				
				var fromRulesPatterns  = fromRules.map(x => translateRule(findMatchingGrammarRule(x,  matchedRules, symbolTable), x, symbolTable))
				var   toRulesPatterns  =   toRules.map(x => translateRule(findMatchingGrammarRule(x, producedRules, symbolTable), x, symbolTable))
				var currentFromPattern = fromRulesPatterns.head
				var currentToPattern   = toRulesPatterns.head  
				var insertInFrom       = fromRulesPatterns.tail
				var insertInTo         = toRulesPatterns.tail
				for(currentInsert <- insertInFrom){
					currentFromPattern = replaceAll(currentFromPattern, currentInsert)
				}
				for(currentInsert <- insertInTo){
					currentToPattern = replaceAll(currentToPattern, currentInsert)
				}
				if(getTags(currentFromPattern) == getTags(currentToPattern)){
					//println("--------------")
					//println("success!")
					//println("fromRules:")
					//fromRules.foreach(println)
					//println(found)
					//println("toRules:")
					//toRules.foreach(println)
					//println(needed)
					res += PatternSynonym(currentFromPattern, currentToPattern)
					//println(res.last)
					//println("--------------\n\n")
				}
				else {
					//println("--------------")
					//println("failure :(")
					//println("fromRules:")
					//fromRules.foreach(println)
					//println("toRules:")
					//toRules.foreach(println)
					//println("pattern:")
					//println(PatternSynonym(currentFromPattern, currentToPattern))
					//println("from/to Vars:")
					//println(getTags(currentFromPattern))
					//println(getTags(currentToPattern))
					//println("--------------\n\n")
				}
				
			//}
		}
		res.toList			
	}
	
	def findMatchingGrammarRule(what: GrammarRuleMatcher, in: GrammarRules, symbolTable: SymbolTable): GrammarRule = in.filter(matches(symbolTable, _, what) != None).head
	
	def replaceAll(in: TypedPattern, what: TypedPattern): TypedPattern = {
		val newContent = in.patternContent.map(x => x match {
			case TypedPatternVariable(_, typ) if(typ == what.typ) => what
			case t:TypedPattern => replaceAll(t, what)
			case x => x 	
		})
		TypedPattern(in.patternName, newContent, in.typ)
	}
	
	def matching(from: GrammarRuleMatcher, to: GrammarRuleMatcher): Boolean = {
		var any = false
		for(toAtom <- to.rhs){
			if(toAtom.tag != "" && !(names(from)(toAtom))) return false
			else if(toAtom.tag != "") any = true
		}
		return any
	}
	
	def names(from: GrammarRuleMatcher)(toAtom: TransformerAtom): Boolean = {
		for(fromAtom <- from.rhs){ 
			if(fromAtom.tag == toAtom.tag) return true
		}
		return false
	}
	
	def translateRule(matchedRule: GrammarRule, grammarRuleMatcher:  GrammarRuleMatcher, symbolTable: SymbolTable): TypedPattern = {
		TypedPattern(matchedRule.tag, grammarRuleMatcher.rhs.map(translatePatternAtom(symbolTable)), symbolTable(grammarRuleMatcher.lhs.name))
	}
	
	def translatePatternAtom(symbolTable: SymbolTable)(x: TransformerAtom): PatternAtom =  x match {
			case NonterminalMatcher(id, tag) => TypedPatternVariable(tag, symbolTable(id))
			case LiteralMatcher(m, tag) => PatternTerminal(tag, m)
			case TerminalMatcher(id, tag) => PatternTerminal(tag, symbolTable(id).name)
			case IntegerMatcher(tag) => PatternTerminal(tag, "<int>")
		}
	
	
	
	
}