================================================================
Laws of obviously bidirectional grammar/datatype transformations
================================================================

Let's investigate what grammar transformations are obviously
bidirectional, and derive the information necessary to generate
syntax tree transformations from obviously bidirectional grammar
transformations.


Example
-------

As a running example, consider the transformation between the
concrete and abstract grammars of arithmetic expressions:

    Concrete := Summand                    [C1]
    Concrete := Concrete "+" Summand       [C2]
     Summand := Factor                     [C3]
     Summand := Summand "*" Factor         [C4]
      Factor := <number>                   [C5]
      Factor := "(" Concrete ")"           [C6]

    Abstract := Abstract "+" Abstract      [A2]
    Abstract := Abstract "*" Abstract      [A4]
    Abstract := <number>                   [A5]

This grammar transformation corresponds to conversion between two
datatypes. First, let us postulate the symbol literals for
addition, multiplication and parentheses.

> data Add = Add deriving Show
> data Mul = Mul deriving Show
> data LP  = LP  deriving Show
> data RP  = RP  deriving Show

The concrete grammar is a series of mutually recursive datatypes.

> data Concrete = C1 Summand
>               | C2 Concrete Add Summand
>               deriving Show
>
> data Summand  = C3 Factor
>               | C4 Summand Mul Factor
>               deriving Show
>
> data Factor   = C5 Integer
>               | C6 LP Concrete RP
>               deriving Show

The abstract grammar is one datatype.

> data Abstract = A2 Abstract Add Abstract
>               | A4 Abstract Mul Abstract
>               | A5 Integer
>               deriving Show

Note the correspondance between rules C2 and A2, C4 and A4, C5
and A5. They give us directly the conversion from corresponding
syntax trees.

The rules C1, C3, C6 vanish in the abstract grammar. In fact,
they correspond to the following trivial production:

    Abstract := Abstract                   [A0]

C1 and C3 directly translate to A0. The rule C6 becomes A0 by
deleting the children "(" and ")".

> abstractC :: Concrete -> Abstract
> abstractC (C1 summand)              = abstractS summand
> abstractC (C2 concrete Add summand) = A2 (abstractC concrete) Add (abstractS summand)
>
> abstractS :: Summand  -> Abstract
> abstractS (C3 factor)               = abstractF factor
> abstractS (C4 summand Mul factor)   = A4 (abstractS summand) Mul (abstractF factor)
>
> abstractF :: Factor   -> Abstract
> abstractF (C5 integer)              = A5 integer
> abstractF (C6 LP concrete RP)       = abstractC concrete

[TO BE CONTINUED]
