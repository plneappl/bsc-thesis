object ReadableSyntaxGrammar {
  import Grammar._
  import Transform._
  import sext._
  
  val List(commentNL, start, doc, ruleDef, inPart, outPart, sequencePart, ruleMatchers, ruleMatcher, rhs, rhsAtom, term, nt, literal, int, recursive, identifier) = 
    List('commentNL, 'start, 'doc, 'ruleDef, 'inPart, 'outPart, 'sequencePart, 'ruleMatchers, 'ruleMatcher, 'rhs, 'rhsAtom, 'term, 'nt, 'literal, 'int, 'recursive, 'identifier).map(Nonterminal)
  
  val t_start        = Terminal("""start:"""                      )
  val t_commentNL      = Regex(   """//[^\r\n]*"""                )
  val t_in          = Regex(   """in:"""                          )
  val t_out         = Regex(   """out:"""                         )
  val t_seq         = Regex(   """seq:"""                         )
  val t_rightArrow  = Terminal("""->"""                           )
  val t_colon       = Terminal(""":"""                            )
  val t_pipe        = Terminal("""|"""                            )
  val t_nt          = Regex(   """[A-Z][A-Za-z0-9]*"""            )
  val t_literal     = Regex(   """"([^A-Z"|<:\s][^"|<:\s]*)?""""  )                  //")//this is so my syntax highlighting works
  val t_term        = Regex(   """[a-z][A-Za-z0-9]*"""            )
  val t_identifier  = Regex(   """[a-z]+"""                       )
  val t_num         = Regex(   """[0-9]+"""                       )
  val t_int         = Terminal("""<int>"""                        )
  val t_optSpace    = Regex(   """ *"""                           )
  val t_space       = Regex(   """ +"""                           )
  val t_newLines    = Regex(   """[\s\r\n]+"""                    )
  val t_optNewLines = Regex(   """[\s\r\n]*"""                    )
  val t_lbrace      = Terminal("""("""                            )
  val t_rbrace      = Terminal(""")"""                            )
  val t_recursive    = Terminal("""r"""                           )
  
  val rules = (List(
    GrammarRule(commentNL   , List(t_newLines, t_commentNL, t_newLines, commentNL                                       ), 0),
    GrammarRule(commentNL   , List(t_newLines, t_commentNL, t_newLines                                                  ), 0),
    GrammarRule(commentNL   , List(t_commentNL, t_newLines, commentNL                                                   ), 0),
    GrammarRule(commentNL   , List(t_commentNL, t_newLines                                                              ), 0),
    GrammarRule(commentNL   , List(t_newLines                                                                           ), 0),
    GrammarRule(start       , List(t_optNewLines, t_start, commentNL, nt, commentNL, doc                                ), 0),
    GrammarRule(doc         , List(ruleDef, doc                                                                         ), 0),                                                                                                                                                           
    GrammarRule(doc         , List(ruleDef                                                                              ), 0),                                                                                                                                                                
    GrammarRule(ruleDef     , List(inPart, outPart                                                                      ), 0),                                                                                                                                                                 
    GrammarRule(ruleDef     , List(inPart, sequencePart                                                                 ), 0),                                                                                                              
    GrammarRule(inPart      , List(t_in, commentNL, ruleMatchers                                                        ), 0),                                                                                                                                                  
    GrammarRule(outPart     , List(t_out, commentNL, ruleMatchers                                                       ), 0),                                                                                                                                                    
    GrammarRule(sequencePart, List(t_seq, commentNL, ruleMatchers                                                       ), 0),                                                                                                                                                       
    GrammarRule(ruleMatchers, List(ruleMatcher, ruleMatchers                                                            ), 0),                                                                                                                                                       
    GrammarRule(ruleMatchers, List(ruleMatcher                                                                          ), 0),                                                                                                                                               
    GrammarRule(ruleMatcher , List(nt, t_space, t_rightArrow, t_space, rhs, commentNL                                   ), 0),                                                       
    GrammarRule(rhs         , List(rhsAtom, t_space, rhs                                                                ), 0),                                               
    GrammarRule(rhs         , List(rhsAtom                                                                              ), 0),   
    GrammarRule(rhsAtom     , List(recursive                                                                            ), 0),   
    GrammarRule(rhsAtom     , List(nt                                                                                   ), 0),   
    GrammarRule(rhsAtom     , List(term                                                                                 ), 0),   
    GrammarRule(rhsAtom     , List(literal                                                                              ), 0),   
    GrammarRule(rhsAtom     , List(int                                                                                  ), 0),
    GrammarRule(rhsAtom     , List(t_pipe                                                                               ), 0),                                                   
    GrammarRule(recursive   , List(t_recursive, t_lbrace, nt, t_rbrace                                                  ), 0),                                
    GrammarRule(nt          , List(t_nt, identifier                                                                     ), 0),                            
    GrammarRule(nt          , List(t_nt                                                                                 ), 0),                               
    GrammarRule(literal     , List(t_literal, identifier                                                                ), 0),                                                                      
    GrammarRule(literal     , List(t_literal                                                                            ), 0),                                
    GrammarRule(term        , List(t_term, identifier                                                                   ), 0),                                                                      
    GrammarRule(term        , List(t_term                                                                               ), 0),                                                            
    GrammarRule(int         , List(t_int, identifier                                                                    ), 0),                                                            
    GrammarRule(int         , List(t_int                                                                                ), 0),
    GrammarRule(identifier  , List(t_colon, t_num                                                                       ), 0),
    GrammarRule(identifier  , List(t_colon, t_identifier                                                                ), 0)                                                                                                                                        
  ) zip (Stream from 1)).map(x => x match { case (GrammarRule(a, b, _), c) => GrammarRule(a, b, c)})          
  
  def grammar = Grammar(start, rules)
  
  def parseReadableSyntax = parseWithGrammar(grammar) _
  
  type SyntaxTreeRecurser[A] = List[SyntaxTree] => Option[A]
  
  def operateOnRRTree[A](s: Symbol)(f: SyntaxTreeRecurser[A])(t: SyntaxTree): List[A] = t match {
    case Branch(_, list) => list.map(x => x match { 
      case Branch(s1, list1) if(s1 == s) => f(list1)
      case x => None
    }).flatten ++ list.map(operateOnRRTree(s)(f)).flatten
    case _ => List()
  }
  
  //GrammarRule(start       , List(t_optNewLines, t_start, commentNL, nt, commentNL, doc 
  def docToTransformerRules(t: SyntaxTree): GrammarTransformer = t match {
    case Branch('start, List(_, _, _, Branch('nt, List(LeafString(nt))), _, doc)) => { 
      GrammarTransformer(
        NonterminalMatcher(nt, ""), 
        splitRules(operateOnRRTree('ruleDef)(ruleDefsToTransformerRules)(doc))
      )
    }
  }
  
  def splitRules(t: TransformerRules): TransformerRules = t.map(
      x => TransformerRule(x.from.map(splitMatcher).flatten, x.to.map(splitMatcher).flatten)
  )
  def splitMatcher(t: GrammarRuleMatcher): List[GrammarRuleMatcher] = {
    var (t1, t2) = t.rhs.span(x => !x.isInstanceOf[LiteralMatcher] || x.asInstanceOf[LiteralMatcher].matches != "|")
    var res = List(GrammarRuleMatcher(t.lhs, t1))
    while(t2 != Nil){
      var (t11, t21) = t2.tail.span(x => !x.isInstanceOf[LiteralMatcher] || x.asInstanceOf[LiteralMatcher].matches != "|")
      t2 = t21
      res = GrammarRuleMatcher(t.lhs, t11) :: res
    }
    res
  }
  
  def ruleDefsToTransformerRules: SyntaxTreeRecurser[TransformerRule] = t => t match {
    case List(in, out) => {
      var res = TransformerRule(
        operateOnRRTree('ruleMatcher)(ruleMatcherToGrammarRuleMatcher)(in),
        operateOnRRTree('ruleMatcher)(ruleMatcherToGrammarRuleMatcher)(out)  
      )
      if(out.asInstanceOf[Branch].childs(0).asInstanceOf[LeafString].str.startsWith("seq:")){
        val usedTags = (res.from ++ res.to).map(r => getUsedTags(r.rhs)).flatten
        var i = 1
        var from = for(rule <- res.from) yield {
          rule.copy(rhs = for(atom <- rule.rhs) yield {
            if(atom.tag == ""){
              i += 1
              while(usedTags.contains(i)) i += 1
              atom.copy(i.toString)
            }
            else atom
          })
        }
        i = 1
        var to = for(rule <- res.to) yield {
          rule.copy(rhs = for(atom <- rule.rhs) yield {
            if(atom.tag == ""){
              i += 1
              while(usedTags.contains(i)) i += 1
              atom.copy(i.toString)
            }
            else atom
          })
        }
        res = TransformerRule(from, to)
      }
      Some(res)
    }
  }
  
  val getUsedTags: List[TransformerAtom] => Set[String] = x => x.map(y => {if(y.tag != "") Some(y.tag) else None}).flatten.toSet
  
  //GrammarRule(ruleMatcher , List(nt, t_optSpace, t_rightArrow, t_optSpace, rhs, commentNL                                ), 0),                                                         
  def ruleMatcherToGrammarRuleMatcher: SyntaxTreeRecurser[GrammarRuleMatcher] = t => { 
    t match {
      case List(Branch('nt, List(nt)), _, _, _, rhs, _) => {
        Some(GrammarRuleMatcher(
          NonterminalMatcher(nt.asInstanceOf[LeafString].str, ""), 
          operateOnRRTree('rhsAtom)(rhsAtomToTransformerAtom)(rhs)
        ))
      }
    }
  }
  
   
  
  def rhsAtomToTransformerAtom: SyntaxTreeRecurser[TransformerAtom] = t => t.head match {
    case Branch('recursive, list)  => handleRecursive(list) 
    case Branch('nt       , list)  => handleNT(list)
    case Branch('term     , list)  => handleTerm(list)      
    case Branch('literal  , list)  => handleLiteral(list)
    case Branch('int      , list)  => handleInt(list)
    case LeafString("|")           => Some(LiteralMatcher("|", ""))
  }
  
  //GrammarRule(recursive   , List(t_recursive, t_rbrace, nt, t_lbrace                                    
  def handleRecursive: SyntaxTreeRecurser[TransformerAtom] = t => t match{
    case List(_, _, Branch('nt, nt), _) => handleNT(nt)
    case _ => None
  }

  //GrammarRule(nt          , List(t_nt, identifier                                 
  //GrammarRule(nt          , List(t_nt                                                
  def handleNT: SyntaxTreeRecurser[TransformerAtom] = t => {
    val ntpattern =      ("(" + t_nt.sym + ")").r
    t.head match {
      case LeafString(t1) => t1 match {
        case ntpattern(nt) => handleIdentifier(NonterminalMatcher(nt, ""))(t.tail)
      }
      case _ => None
    }
  }
  
  //GrammarRule(literal     , List(t_literal, identifier                                                                      
  //GrammarRule(literal     , List(t_literal                                            
  def handleLiteral: SyntaxTreeRecurser[TransformerAtom] = t => {
    val literalpattern = ("(" + t_literal.sym + ")").r
    t.head match {
      case LeafString(t1) => t1 match {
        case literalpattern(lit) => handleIdentifier(LiteralMatcher(lit, ""))(t.tail)
      }
      case _ => None
    }
  }
  
  //GrammarRule(term        , List(t_term, identifier                                                                        
  //GrammarRule(term        , List(t_term                       
  def handleTerm: SyntaxTreeRecurser[TransformerAtom] = t => {
    val termpattern =    ("(" + t_term.sym + ")").r
    t.head match {
      case LeafString(t1) => t1 match {
        case termpattern(term) => handleIdentifier(TerminalMatcher(term, ""))(t.tail)
      }
      case _ => None
    }
  }
                                                    
  //GrammarRule(int         , List(t_int, identifier                                                               
  //GrammarRule(int         , List(t_int               
  def handleInt: SyntaxTreeRecurser[TransformerAtom] = t => {
    t.head match {
      case LeafString(t1) if(t1 == "<int>") => handleIdentifier(IntegerMatcher(""))(t.tail)
    }
  }
  
  //GrammarRule(identifier  , List(t_colon, IntegerTerminal
  //GrammarRule(identifier  , List(t_colon, t_identifier                                                                                                                                          
  def handleIdentifier(x: TransformerAtom): SyntaxTreeRecurser[TransformerAtom] = t => t match {
    case Nil => Some(x)
    case List(Branch('identifier, List(_, LeafString(st)))) => Some(x.copy(st))
  }
    
  def getGrammarTransformer(path: String): GrammarTransformer = {
    val source = scala.io.Source.fromFile(path)
    val tr = source.mkString
    source.close
    val tr2 = (parseWithGrammar(grammar)(tr))
    //println(tr2.treeString)
    docToTransformerRules(tr2)
  }
}