/** Bidirectional grammar transformation:
  * Grammar is to syntax trees as datatype is to its inhabitants.
  *
  * Usage for grammars:
  *
  * - Chomsky normal form
  * - inlining of rules at a position until all left-recursions are explicit
  * - left-recursion elimination
  * - left-factoring
  * - removing explicit parentheses
  * - remove explicit precedence from grammar
  *
  * Usage for datatypes:
  *
  * - convert mutually recursive datatypes to singly recursive datatypes
  *
  * Transformations:
  *
  * - naming (inverse of inlining): BNF
  * - inlining (inverse of naming): mutual recursion elimination
  * - multiple naming: left recursion elimination, left factoring
  * - deleting with representative: parentheses removal
  * - forgeting with parentheses as way back
  */


object Grammar extends App {
  // context-free grammar
  type CFG = List[(Atom, List[Atom])]

  trait Atom extends Construct
  trait Construct
  case class Select(children: Construct*) extends Construct
  case class Concat(children: Construct*) extends Construct
  case class Repeat(children: Construct*) extends Construct

  // abstract syntax tree
  sealed trait Tree { def tag: Atom }
  case class Leaf(tag: Atom, get: String) extends Tree
  case class Fork(tag: Atom, children: Tree*) extends Tree

  // test if an atom is a terminal symbol in a context-free grammar
  def isTerminal(atom: Atom, cfg: CFG): Boolean =
    ! cfg.exists(_._1 == atom)

  // test that a tree belongs to the language of a grammar
  def belongs(tree: Tree, cfg: CFG): Boolean = tree match {
    case Leaf(tag, _) =>
      isTerminal(tag, cfg)

    case Fork(tag, children @ _*) =>
      cfg.exists {
        case (atom, construct) =>
          atom == tag && construct.size == children.size && {
            val result = children.zip(construct).map {
              case (child, atom) =>
                child.tag == atom && belongs(child, cfg)
            }
            result.isEmpty || result.min
          }
      }
  }

  // EXAMPLE: removing explicit parenthesis
  case object Exp    extends Atom
  case object Num    extends Atom
  case object Plus   extends Atom
  case object LParen extends Atom
  case object RParen extends Atom

  val input1: CFG = List(
    Exp -> List(Num),
    Exp -> List(LParen, Exp, Plus, Exp, RParen)
  )

  val output1 = List(
    Exp -> List(Num),
    Exp -> List(Exp, Plus, Exp)
  )

  val transform1: CFG => CFG = _ map {
    case (Exp, List(LParen, Exp, Plus, Exp, RParen)) =>
      Exp -> List(Exp, Plus, Exp)

    case other => other
  }

  assert(transform1(input1) == output1)

  lazy val forward1: Tree => Tree = {
    case Fork(
      Exp,
      Leaf(LParen, _), lhs @ Fork(Exp, _*), plus @ Leaf(Plus, _),
      rhs @ Fork(Exp, _*), Leaf(RParen, _)
    ) =>
      Fork(Exp, List(lhs, plus, rhs) map forward1: _*)

    case Fork(tag, children @ _*) =>
      Fork(tag, children map forward1: _*)

    case Leaf(tag, get) =>
      Leaf(tag, get)
  }

  val lparen = Leaf(LParen, "(")
  val rparen = Leaf(RParen, ")")
  val plus   = Leaf(Plus  , "+")

  def int2exp(n: Int): Tree = Fork(Exp, Leaf(Num, n.toString))

  lazy val backward1: Tree => Tree = {
    case Fork(Exp,
      lhs @ Fork(Exp, _*), plus @ Leaf(Plus, _), rhs @ Fork(Exp, _*)
    ) =>
      Fork(Exp, List(lparen, lhs, plus, rhs, rparen) map backward1: _*)

    case Fork(tag, children @ _*) =>
      Fork(tag, children map backward1: _*)

    case Leaf(tag, get) =>
      Leaf(tag, get)
  }

  val ti1: Tree = Fork(Exp,
    lparen,
    Fork(Exp, lparen, int2exp(1), plus, int2exp(2), rparen),
    plus,
    Fork(Exp, lparen, int2exp(3), plus, int2exp(4), rparen),
    rparen
  )

  val to1: Tree = Fork(Exp,
    Fork(Exp, int2exp(1), plus, int2exp(2)),
    plus,
    Fork(Exp, int2exp(3), plus, int2exp(4))
  )

  assert(  belongs(ti1, input1 ))
  assert(! belongs(ti1, output1))
  assert(  belongs(to1, output1))
  assert(! belongs(to1, input1 ))

  assert(forward1 (ti1) == to1)
  assert(backward1(to1) == ti1)

  println("All assertions succeeded.")
}
