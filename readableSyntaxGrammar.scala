object ReadableSyntaxGrammar {
	import Grammar._
	
	val List(doc, ruleDef, inPart, afterIn, outPart, sequencePart, deletePart, ruleMatchers, ruleMatcher, rhs, rhsAtom) = 
		List('doc, 'ruleDef, 'inPart, 'afterIn, 'outPart, 'sequencePart, 'deletePart, 'ruleMatchers, 'ruleMatcher, 'rhs, 'rhsAtom).map(Nonterminal)
	
	val t_in          = Regex(   """in:\s*[\n\r]{1,2}""" )
	val t_out         = Regex(   """out:\s*[\n\r]{1,2}""")
	val t_seq         = Regex(   """seq:\s*[\n\r]{1,2}""")
	val t_del         = Regex(   """del:\s*[\n\r]{1,2}""")
	val t_rightArrow  = Terminal("""->"""                )
	val t_colon       = Terminal(""":"""                 )
	val t_pipe        = Terminal("""|"""                 )
	val nt            = Regex(   """[A-Z][A-Za-z]*"""    )
	val term          = Regex(   """[^A-Z|<:\s]+"""      )
	val t_int         = Terminal("""<int>"""             )
	val t_optSpace    = Regex(   """ *"""                )
	val t_space       = Regex(   """ +"""                )
	val t_newLines		= Regex(   """[\s\r\n]+"""         )
	val t_optNewLines = Regex(   """[\s\r\n]*"""         )
	
	val rules = (List(
		GrammarRule(doc         , List(ruleDef, doc                                             ), 0),                                                                                                                                                           
		GrammarRule(doc         , List(ruleDef                                                  ), 0),                                                                                                                                                                
		GrammarRule(ruleDef     , List(inPart, afterIn                                          ), 0),                                                                          
		GrammarRule(afterIn     , List(outPart                                                  ), 0),                                                                         
		GrammarRule(afterIn     , List(sequencePart, deletePart                                 ), 0),                                                                                                                                                                                    	                                                                                                  
		GrammarRule(afterIn     , List(sequencePart                                             ), 0),                                         
		GrammarRule(inPart      , List(t_in, ruleMatchers                                       ), 0),                                                                                                                                                	
		GrammarRule(outPart     , List(t_out, ruleMatchers                                      ), 0),                                                                                                                                                  	
		GrammarRule(sequencePart, List(t_seq, ruleMatchers                                      ), 0),                                                                                                                                                       
		GrammarRule(deletePart  , List(t_del, ruleMatchers                                      ), 0),                                                                                                                                                    	
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
	
	def grammar = Grammar(doc, rules)
	
	def parseReadableSyntax = parseWithGrammar(grammar) _
}