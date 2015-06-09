object Transform {
	
	import Grammar._
	import sext._
	import collection.mutable.{ ListBuffer, HashMap, MultiMap, Set => MSet }
	
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
		
	sealed abstract class TransformerAtom{
		def tag: Int
		def copy(t: Int): TransformerAtom
	}
	case class NonterminalMatcher(name: String, tag: Int) extends TransformerAtom {
		override def toString = name +  (if(tag != -1) ":" + tag else "")
		def copy(t: Int) = NonterminalMatcher(name, t)
	}
	case class TerminalMatcher(name: String, tag: Int) extends TransformerAtom {
		override def toString = if(tag >= 0) name + ":" + tag else name
		def copy(t: Int) = TerminalMatcher(name, t)
	}
	case class LiteralMatcher(matches: String, tag: Int) extends TransformerAtom {
		override def toString = if(tag >= 0) s""""$matches":$tag""" else s""""$matches""""               //"//again, for syntax highlighting
		def copy(t: Int) = LiteralMatcher(matches, t)
	}
	case class IntegerMatcher(tag: Int) extends TransformerAtom {
		override def toString = "<int>:" + tag
		def copy(t: Int) = IntegerMatcher(t)
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
	
	//C to A: take C -> C + S, C -> S and produce A -> A + A
	val transformCtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher("B", 0), List(NonterminalMatcher("C", 1), LiteralMatcher("+", 2), NonterminalMatcher("B", 3))),
			GrammarRuleMatcher(NonterminalMatcher("B", 0), List(NonterminalMatcher("C", -1)))
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher("A", 0), List(NonterminalMatcher("A", 1), LiteralMatcher("+", 2), NonterminalMatcher("A", 3)))
		)
	)
	
	//S to A: take S -> S * F, S -> F and produce A -> A * A
	val transformStoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher("C", 0), List(NonterminalMatcher("D", 1), LiteralMatcher("*", 2), NonterminalMatcher("C", 3))),
			GrammarRuleMatcher(NonterminalMatcher("C", 0), List(NonterminalMatcher("D", -1)))
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher("A", 0), List(NonterminalMatcher("A", 1), LiteralMatcher("*", 2), NonterminalMatcher("A", 3)))
		)
	)

	
	//F to A: take F -> [ C ], F -> int and produce A -> int
	val transformFtoA = TransformerRule(
		List(
			GrammarRuleMatcher(NonterminalMatcher("D", 0), List(LiteralMatcher("[", -1), NonterminalMatcher("B", -1), LiteralMatcher("]", -1))),
			GrammarRuleMatcher(NonterminalMatcher("D", 0), List(IntegerMatcher(1)))	
		),
		List(
			GrammarRuleMatcher(NonterminalMatcher("A", 0), List(IntegerMatcher(1)))
		)
	)
	
	//NonterminalMatchers can identify themselves with any symbol, this stores the identification while transforming
	type SymbolTable = Map[String, Symbol]
	
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
		
		for((st, gRules) <- matchedRules){
			var symbolTable = st
			var newRules: GrammarRules = List()
			for(rule <- transformerRules.to){
				val (nst, r) = makeOutRule(symbolTable, rule)
				symbolTable = nst
				newRules = r :: newRules
				
			}
			out = (gRules, symbolTable, newRules) :: out
			
			
		}
		//return produced rules and the SymbolTables
		Some(out)
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
	def applyMatcher(st: SymbolTable, a: TransformerAtom): (SymbolTable, GrammarAtom) = {
		 a match {
			case NonterminalMatcher(id, _) => {
				val st2 = extendSymbolTable(st, id)
				
				(st2, Nonterminal(st2(id)))
			}
			case TerminalMatcher(id, _) => (st, Terminal(st(id).name))

			case LiteralMatcher(str, _) => (st, Terminal(str))
			case IntegerMatcher(_) => (st, IntegerTerminal)
		}
	}
	
	//insert a new Symbol with the target ID
	def extendSymbolTable(st: SymbolTable, name: String): SymbolTable = {
		
		if(st.isDefinedAt(name)) return st
		if(!st.values.toList.contains(Symbol(name))) return st + ((name, Symbol(name)))
		var next = "A"
		while(st.values.toList.contains(Symbol(next))){
			next = nextLexicographic(next)
		}
		st + ((name, Symbol(next)))
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
	
	def concreteToAbstract = GrammarTransformer(NonterminalMatcher("A", 0), List(transformFtoA, transformCtoA, transformStoA))	
	
	
		
	case class PatternSynonym(lhs: TypedPattern, rhs: TypedPattern){
		override def toString = lhs.toString + " = " + rhs.toString
	}
	type PatternSynonyms = List[PatternSynonym]
	sealed trait PatternAtom{ def id: Int; def getIds: Set[Int] }
	case class TypedPatternVariable(id: Int, typ: Symbol) extends PatternAtom{
		override def toString = "(" + id + " :: " + typ.name + ")"
		def getIds = Set(id)
	}
	case class PatternTerminal(id: Int, str: String) extends PatternAtom{
		override def toString = "(" + id + " :: " + str + ")"
		def getIds = Set(id)
	}

	case class TypedPattern(patternName: Int, patternContent: List[PatternAtom], typ: Symbol) extends PatternAtom {
		def id = -1
		def getIds = patternContent.map(_.getIds).fold(Set())((x, y) => x ++ y)
		override def toString = "(" + typ.name + patternName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ.name + ")"
	}
	//Name as String
	//case class TypedPattern(ruleName: String, patternContent: List[PatternAtom], typ: Symbol){
	//	override def toString = ruleName + ":" + patternContent.fold("")(joinStringsBy(" ")) + " :: " + typ
	//}
	
	type SeenTags = MultiMap[Int, TransformerSequence]
	def newSeenTags():SeenTags = new HashMap[Int, MSet[TransformerSequence]] with MultiMap[Int, TransformerSequence]
	def addTags(rule: TransformerSequence)(to: SeenTags): SeenTags = {		
		var res = to
		rule.map(_.tag).filter(_ > -1).foreach(x => res.addBinding(x, rule))
		res
	}	
	
	
	def pairs(max1: Int, max2: Int) = for(i <- (1 to (max1 * max2)).view; j1 <- (1 to max1).view; if(i - j1 >= 1 && i - j1 <= max2)) yield (j1, i - j1)
	
	def getTags(p: TypedPattern): Set[Int] = {
		p.getIds.filter(_ > -1).toSet[Int]
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
			if(toAtom.tag >= 0 && !(names(from)(toAtom))) return false
			else if(toAtom.tag >= 0) any = true
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