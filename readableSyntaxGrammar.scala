object ReadableSyntaxGrammar {
	import Grammar._
	import Transform._
	
	val List(start, doc, ruleDef, inPart, outPart, sequencePart, ruleMatchers, ruleMatcher, rhs, rhsAtom) = 
		List('start, 'doc, 'ruleDef, 'inPart, 'outPart, 'sequencePart, 'ruleMatchers, 'ruleMatcher, 'rhs, 'rhsAtom).map(Nonterminal)
	
	val t_start				= Terminal("""start:"""              )
	val t_in          = Regex(   """in:\s*[\n\r]{1,2}"""   )
	val t_out         = Regex(   """out:\s*[\n\r]{1,2}"""  )
	val t_seq         = Regex(   """seq:\s*[\n\r]{1,2}"""  )
	val t_rightArrow  = Terminal("""->"""                  )
	val t_colon       = Terminal(""":"""                   )
	val t_pipe        = Terminal("""|"""                   )
	val nt            = Regex(   """[A-Z][A-Za-z]*"""      )
	val term          = Regex(   """[^A-Z|<:\s][^|<:\s]*""")
	val t_int         = Terminal("""<int>"""               )
	val t_optSpace    = Regex(   """ *"""                  )
	val t_space       = Regex(   """ +"""                  )
	val t_newLines		= Regex(   """[\s\r\n]+"""           )
	val t_optNewLines = Regex(   """[\s\r\n]*"""           )
	
	val rules = (List(
		GrammarRule(start       , List(t_optNewLines, t_start, t_newLines, nt, t_newLines, doc  ), 0),
		GrammarRule(doc         , List(ruleDef, doc                                             ), 0),                                                                                                                                                           
		GrammarRule(doc         , List(ruleDef                                                  ), 0),                                                                                                                                                                
		GrammarRule(ruleDef     , List(inPart, outPart                                          ), 0),                                                                                                                                                                 
		GrammarRule(ruleDef     , List(inPart, sequencePart                                     ), 0),                                                                                                              
		GrammarRule(inPart      , List(t_in, ruleMatchers                                       ), 0),                                                                                                                                                	
		GrammarRule(outPart     , List(t_out, ruleMatchers                                      ), 0),                                                                                                                                                  	
		GrammarRule(sequencePart, List(t_seq, ruleMatchers                                      ), 0),                                                                                                                                                       
		GrammarRule(ruleMatchers, List(ruleMatcher, ruleMatchers                                ), 0),                                                                                                                                                     	
		GrammarRule(ruleMatchers, List(ruleMatcher                                              ), 0),                                                                                                                                             	
		GrammarRule(ruleMatcher , List(nt, t_optSpace, t_rightArrow, t_optSpace, rhs, t_newLines), 0),                                                       
		GrammarRule(rhs         , List(rhsAtom, t_space, rhs                                    ), 0),                                               
		GrammarRule(rhs         , List(rhsAtom                                                  ), 0),                                      
		GrammarRule(rhsAtom     , List(nt,   t_colon, IntegerTerminal                           ), 0),                             
		GrammarRule(rhsAtom     , List(nt                                                       ), 0),                               
		GrammarRule(rhsAtom     , List(term, t_colon, IntegerTerminal                           ), 0),                                                                      
		GrammarRule(rhsAtom     , List(term                                                     ), 0),                                                            
		GrammarRule(rhsAtom     , List(t_int, t_colon, IntegerTerminal                          ), 0),                                                            
		GrammarRule(rhsAtom     , List(t_int                                                    ), 0),                                                                                                                                           
		GrammarRule(rhsAtom     , List(t_pipe                                                   ), 0)                                                    
	) zip (Stream from 1)).map(x => x match{ case (GrammarRule(a, b, _), c) => GrammarRule(a, b, c)})          
	
	def grammar = Grammar(start, rules)
	
	def parseReadableSyntax = parseWithGrammar(grammar) _
	
	type SyntaxTreeRecurser[A] = SyntaxTree => Option[A]
	
	def operateOnRRTree[A](s: Symbol)(f: SyntaxTreeRecurser[A])(t: SyntaxTree): List[A] = t match {
		case Branch(sym, list) => list.map(f).flatten ++ list.map(operateOnRRTree(s)(f)).flatten
		case _ => List()
	}
		
	def docToTransformerRules(t: SyntaxTree): GrammarTransformer = t match {
		case Branch('start, List(_, _, _, LeafString(nt), _, doc)) => { 
			GrammarTransformer(
				NonterminalMatcher(nt, -1), 
				splitRules(operateOnRRTree('doc)(ruleDefsToTransformerRules)(doc))
			)
		}
	}
	
	def splitRules(t: TransformerRules): TransformerRules = t.map(
			x => TransformerRule(x.from.map(splitMatcher).flatten, x.to.map(splitMatcher).flatten)
	)
	def splitMatcher(t: GrammarRuleMatcher): List[GrammarRuleMatcher] = {
		var (t1, t2) = t.rhs.span(x => x != TerminalMatcher("|", -1))
		var res = List(GrammarRuleMatcher(t.lhs, t1))
		while(t2 != Nil){
			var (t11, t21) = t2.tail.span(x => x != TerminalMatcher("|", -1))
			t2 = t21
			res = GrammarRuleMatcher(t.lhs, t11) :: res
		}
		res
	}
	
	def ruleDefsToTransformerRules: SyntaxTreeRecurser[TransformerRule] = t => t match {
		case Branch('ruleDef, List(in, out)) => {
			Some(TransformerRule(
				operateOnRRTree('ruleMatchers)(ruleMatcherToGrammarRuleMatcher)(in),
				operateOnRRTree('ruleMatchers)(ruleMatcherToGrammarRuleMatcher)(out)	
			))
		}
		case _ => None
	}
	
	def ruleMatcherToGrammarRuleMatcher: SyntaxTreeRecurser[GrammarRuleMatcher] = t => { 
		t match {
			case Branch('ruleMatcher, List(nt, _, _, _, rhs, _)) => {
				Some(GrammarRuleMatcher(
					NonterminalMatcher(nt.asInstanceOf[LeafString].str, -1), 
					operateOnRRTree('rhs)(rhsAtomToTransformerAtom)(rhs)
				))
			}
			case _ => None
		}
	}
	
	def rhsAtomToTransformerAtom: SyntaxTreeRecurser[TransformerAtom] = t => t match {
		case Branch('rhsAtom, List(LeafString(t1), _, LeafInteger(i))) => LeafStringToTransformerAtom(i)(t1)
		case Branch('rhsAtom, List(LeafString(t1))) => LeafStringToTransformerAtom(-1)(t1)
		case _ => None
	}
	
	def LeafStringToTransformerAtom(i: Int): String => Option[TransformerAtom] = t => {
		val ntpattern = """([A-Z][A-Za-z]*)""".r
		val termpattern = """([^A-Z|<:\s][^|<:\s]*)""".r
		t match {
			case ntpattern(nt) => Some(NonterminalMatcher(nt, i))
			case termpattern(term) => Some(TerminalMatcher(term, i))
			case "<int>" => Some(IntegerMatcher(i))
			case "|" => Some(TerminalMatcher("|", -1))
			case _ => None
		}
	}
}