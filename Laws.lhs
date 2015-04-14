================================================================
Laws of obviously bidirectional grammar/datatype transformations
================================================================

Let's investigate what grammar transformations are obviously
bidirectional, and derive the information necessary to generate
syntax tree transformations from obviously bidirectional grammar
transformations.


Grammar-datatype correspondance
-------------------------------

As a running example, consider the transformation between the
concrete and abstract grammars of arithmetic expressions:

    Concrete := Concrete "+" Summand       [C1]
    Concrete := Summand                    [C2]
     Summand := Summand "*" Factor         [C3]
     Summand := Factor                     [C4]
      Factor := <number>                   [C5]
      Factor := "(" Concrete ")"           [C6]

    Abstract := Abstract "+" Abstract      [A1]
    Abstract := Abstract "*" Abstract      [A3]
    Abstract := <number>                   [A5]

This grammar transformation corresponds to conversion between two
datatypes. First, let us postulate the symbol literals for
addition, multiplication and parentheses.

> data Add = Add deriving Show
> data Mul = Mul deriving Show
> data LP  = LP  deriving Show
> data RP  = RP  deriving Show

The concrete grammar is a series of mutually recursive datatypes.

> data Concrete = C1 Concrete Add Summand
>               | C2 Summand
>               deriving Show
>
> data Summand  = C3 Summand Mul Factor
>               | C4 Factor
>               deriving Show
>
> data Factor   = C5 Integer
>               | C6 LP Concrete RP
>               deriving Show

The abstract grammar is one datatype.

> data Abstract = A1 Abstract Add Abstract
>               | A3 Abstract Mul Abstract
>               | A5 Integer
>               deriving Show

Note the correspondance between rules C1 and A1, C3 and A3, C5
and A5. They give us directly the conversion from corresponding
syntax trees.

The rules C2, C4, C6 vanish in the abstract grammar. In fact,
they correspond to the following trivial production:

    Abstract := Abstract                   [A0]

C2 and C4 directly translate to A0. The rule C6 becomes A0 by
deleting the children "(" and ")".

> forwardC :: Concrete -> Abstract
> forwardC (C2 summand)              = forwardS summand
> forwardC (C1 concrete Add summand) = A1 (forwardC concrete) Add (forwardS summand)
>
> forwardS :: Summand -> Abstract
> forwardS (C4 factor)               = forwardF factor
> forwardS (C3 summand Mul factor)   = A3 (forwardS summand) Mul (forwardF factor)
>
> forwardF :: Factor -> Abstract
> forwardF (C5 integer)              = A5 integer
> forwardF (C6 LP concrete RP)       = forwardC concrete

The backward transformation is similar to parsing in two ways.

1. Each node in the abstract syntax tree backward-transforms to
   multiple concrete nodes, based on the needed nonterminal in
   the current context.

2. Ambiguities must be resolved: Since C2, C4, C6 nodes are
   removed in forward transformation, an infinite number of
   concrete syntax trees forward-transform to the same abstract
   syntax tree. During backward transformation, this ambiguity
   must be resolved in some way; we choose to inject as few C6
   nodes as possible.

> backwardC :: Abstract -> Concrete
> backwardC (A1 x Add y) = C1 (backwardC x) Add (backwardS y)
> backwardC other        = C2 (backwardS other)
>
> backwardS :: Abstract -> Summand
> backwardS (A3 x Mul y) = C3 (backwardS x) Mul (backwardF y)
> backwardS other        = C4 (backwardF other)
>
> backwardF :: Abstract -> Factor
> backwardF (A5 integer) = C5 integer
> backwardF other        = C6 LP (backwardC other) RP


Obvious and obviously bidirectional grammar transformations
-----------------------------------------------------------

The transformation is obviously invertible because it does not
lose information.

1. Transforming C1 to A1, C3 to A3, C5 to A5 only shuffles child
   nodes around---each child of C1/C3/C5 corresponds to exactly
   one child in A1/A3/A5.

2. C2 and C4 nodes contributes no information and can be safely
   removed.

3. Transforming C6 removes LP and RP. But since LP and RP are
   inhabited by one value only (i. e., they are unit types),
   it does not lose information to delete them.

Laws to guarantee invertibility:

- Linearity: Each child node in the input production rule appears
  zero or one time in the output production rule.

- In/Del irrelevance: Each added or deleted child node has
  exactly one inhabitant.

Relation to Djinn:

Converting between syntax trees corresponds to generating
functions of type S -> T, where S and T are constructed from
sums and products. However, we don't want just any function;
we want the function that respects the relation between input
and output production rules.

It remains to determine the input format of the syntax tree
transformation generator.
