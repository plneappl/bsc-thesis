% Installation:
% 1. install pdflatex
% 2. install lhs2TeX

% Compilation:
% lhs2TeX master.lhs > master.tex && pdflatex master

\documentclass{amsart}
\usepackage{tikz}

%include lhs2TeX.fmt
%lang haskell
%format pattern = "\mathbf{pattern}"

%subst nested a = "\textrm{ " a " }"
%format . = "."
%format forall = "\ensuremath{\forall}"
%format Gamma  = "\ensuremath{\Gamma}"

\long\def\ignore#1{}
\ignore{
\begin{code}
{-# LANGUAGE RankNTypes #-}
\end{code}
}


\gdef\R{\rightarrow}

\newtheorem{lemma}[subsection]{Lemma}

\begin{document}

\null
\vskip 3cm plus 1cm
\title{Pattern synonyms describe syntax tree transformations}
\maketitle
\vskip 3cm plus 1cm

\section{Syntax tree transformations by example}

Let us see some examples of transformations between grammars.

\subsection{Concrete and abstract grammars for arithmetic expressions}

The input grammar describes the concrete syntax of arithmetic
expressions with operator precedence and parentheses.

Let $C$ be the concrete grammar.
\begin{align*}
C &\R C~+~S & (C_1) \\
C &\R S     & (C_2) \\
S &\R S~\times~F & (S_3) \\
S &\R F     & (S_4) \\
F &\R \mbox{integer} & (F_5) \\
F &\R (~C~) & (F_6)
\end{align*}

Let $A$ be the abstract grammar.
\begin{align*}
A &\R A~+~A & (A_1) \\
A &\R A~\times~A & (A_3) \\
A &\R \mbox{integer} & (A_5)
\end{align*}

Intuitively, nodes labeled $C_1,S_3,F_5$ in concrete syntax trees
gets translated respectively to nodes labeled $A_1,A_3,A_5$ in
abstract syntax trees.

This is the concrete syntax tree of the expression $3\times4+5$.
\[
\begin{tikzpicture}
\path node{$C_1$}
child { node{$C_2$}
  child { node{$S_3$}
    child { node{$S_4$} child { node{$F_5$} child { node{$3$} }}}
    child { node{$\times$} }
    child { node{$F_5$} child { node{$4$} } }
  }
}
child { node{$+$} }
child{ node{$S_4$} child{ node{$F_5$} child{ node{$5$} }}}
;
\end{tikzpicture}
\]

This is the abstract syntax tree of the expression $3\times4+5$.
\[
\begin{tikzpicture}
\path node{$A_1$}
child { node{$A_3$}
  child { node{$A_5$} child { node{$3$} } }
  child { node{$\times$} }
  child { node{$A_5$} child { node{$4$} } }
}
child { node{$+$} }
child { node{$A_5$} child { node{$5$} } }
;
\end{tikzpicture}
\]

\subsection{Chomsky normal form} The input grammar is the
abstract grammar $A$ of arithmetic expressions. The output
grammar $N$ is the Chomsky normal form of $A$.
\begin{align*}
N &\R N~P & (N_1) \\
N &\R N~Q & (N_3) \\
N &\R \mbox{integer} & (N_5) \\
P &\R +~N & (P_1) \\
Q &\R \times~N & (Q_3)
\end{align*}

Intuitively, nodes labeled $A_1,A_3,A_5$ in abstract syntax trees
gets translated respectively to nodes labeled $N_1,N_3,N_5$ in
syntax trees in Chomsky normal form. The production $P_1$ comes
from $A_1$, and the production $Q_3$ comes from $A_3$.

This is the syntax tree of $3\times4+5$ in Chomsky normal form.
\[
\begin{tikzpicture}
\path node{$N_1$}
child { node{$N_3$}
  child { node{$N_5$} child { node{$3$} } }
  %child[draw opacity=0] {}
  child { node{$Q_3$}
    child { node{$\times$} }
    child { node{$N_5$} child { node{$4$} } }
  }
}
child[draw opacity=0] {}
child { node{$P_1$}
  child { node{$+$} }
  child { node{$N_5$} child { node{$5$} } }
};
\end{tikzpicture}
\]


\section{Grammar-datatype correspondence}

Each context-free grammar can be considered as a family of
mutually recursive datatypes. We interpret grammars as datatypes
according to the following table.

\[
\begin{tabular}{rcl}
Grammar & is & family of mutually recursive datatypes \\
Nonterminal symbol & is & datatype \\
Terminal symbol & is & primitive type \\
Production rule & is & data constructor \\
String literal & is & singleton type \\
Syntax tree & is & value of a datatype
\end{tabular}
\]

Following this schema, we translate the grammars to datatypes as
follows.

\gdef\PAR{\bigbreak\noindent}

\PAR
\begin{code}
-- String literals
data Add  =  Add  deriving (Show, Eq)  -- +
data Mul  =  Mul  deriving (Show, Eq)  -- $\times$
data LP   =  LP   deriving (Show, Eq)  -- (
data RP   =  RP   deriving (Show, Eq)  -- )
\end{code}

\PAR
\begin{code}
-- The concrete grammar $C$

data C  =  C1 C Add S
        |  C2 S
        deriving (Show, Eq)

data S  =  S3 S Mul F
        |  S4 F
        deriving (Show, Eq)

data F  =  F5 Int
        |  F6 LP C RP
        deriving (Show, Eq)
\end{code}

\PAR
\begin{samepage}
\begin{code}
-- The abstract grammar $A$

data A  =  A1 A Add A
        |  A3 A Mul A
        |  A5 Int
        deriving (Show, Eq)
\end{code}
\end{samepage}

\PAR
\begin{code}
-- The grammar $N$ in Chomsky 2-normal form

data N  =  N1 N P
        |  N3 N Q
        |  N5 Int
        deriving (Show, Eq)

data P  =  P1 Add N
        deriving (Show, Eq)

data Q  =  Q3 Mul N
        deriving (Show, Eq)
\end{code}

\PAR
\begin{code}
-- Syntax trees of $3\times4+5$ in grammars $C$, $A$ and $N$

ec  ::  C
ec  =   C1  (C2 (S3 (S4 (F5 3)) Mul (F5 4)))
            Add
            (S4 (F5 5))

ea  ::  A
ea  =   A1 (A3 (A5 3) Mul (A5 4)) Add (A5 5)

en  ::  N
en  =   N1 (N3 (N5 4) (Q3 Mul (N5 5))) (P1 Add (N5 2))
\end{code}


\section{Pattern synonyms}

We use \emph{pattern synonyms} to describe information flows between
grammar transformations. A pattern synonym has a left-hand-side and
a right-hand-side, both are patterns in Haskell's pattern-matching.

We require pattern synonyms be \emph{linear}, in that each variable
occurs exactly once on the left-hand-side and exactly once on the
right-hand-side of a pattern synonyms.
Linear pattern synonyms are \emph{invertible}. To invert a pattern
synonym, we swap its left-hand-side with its right-hand-side.

We require type annotation on patterns whose type cannot be inferred.

\PAR These are pattern synonyms of the transformation between $C$
and $A$:

< pattern C1 x Add y  =  A1 x Add y
< pattern C2 x        =  x :: A
< pattern S3 x Mul y  =  A3 x Mul y
< pattern S4 x        =  x :: A
< pattern F5 n        =  A5 n
< pattern F6 LP x RP  =  x :: A

\PAR These are pattern synonyms of the transformation between $A$
and $N$:

< pattern A1 x Add y  =  N1 x (P1 Add y)
< pattern A3 x Mul y  =  N3 x (Q3 Mul y)
< pattern A5 n        =  N5 n


\section{From pattern synonyms to syntax tree transformations}

We convert pattern synonyms to syntax tree transformations as folows.
\begin{enumerate}
\item Order pattern synonyms by inclusion. If there are unordered
patterns with nonempty intersections, then proceed only if those
patterns are consistent.
\item Annotate patterns and \emph{all} occurrences of type variables
by their types in the corresponding production rules.
\item Generate mutually-recursive syntax tree transformations.
\end{enumerate}
We will discuss steps 2 and 3 first, so that the reader knows how
it works as soon as possible. Step 1 is there as a safety measure
and will be discussed last.

\subsection{Fully type-annotated pattern synonyms}

To generate mutually recursive syntax tree transformations, we
need to know the type of \emph{all} occurrences of variables.
We will annotate type synonyms according to the corresponding
production rules. One variable are annotated different types
depending on where it occurs.

\PAR These are fully type-annotated pattern synonyms
of the transformation between $C$ and $A$:

< pattern C1 (x :: C) Add (y :: S)  :: C {--}  = {--}  A1 (x :: A) Add (y :: A)  :: A
< pattern C2 (x :: S)               :: C {--}  = {--}  x :: A                    :: A
< pattern S3 (x :: S) Mul (y :: F)  :: S {--}  = {--}  A3 (x :: A) Mul (y :: A)  :: A
< pattern S4 (x :: F)               :: S {--}  = {--}  x :: A                    :: A
< pattern F5 (n :: Int)             :: F {--}  = {--}  A5 (n :: Int)             :: A
< pattern F6 LP (x :: C) RP         :: F {--}  = {--}  x :: A                    :: A

\PAR These are pattern synonyms of the transformation between $A$
and $N$:

< pattern A1 (x :: A) Add (y :: A)  :: A {--}  = {--}  N1 (x :: N) (P1 Add (y :: N))  :: N
< pattern A3 (x :: A) Mul (y :: A)  :: A {--}  = {--}  N3 (x :: N) (Q3 Mul (y :: N))  :: N
< pattern A5 (n :: Int)             :: A {--}  = {--}  N5 (n :: Int)                  :: N

\subsection{Mutually recursive syntax tree transformations}

A fully type-annotated pattern synonym
\quad
|pattern LHS :: S = RHS :: T|
\quad
compiles to a case the definition of a function of type |S -> T|.
These cases are built from themselves and from the identity function.
The compilation of pattern synonyms is a 3-step process.
\begin{enumerate}
\item Put |id :: forall a. a -> a| into context.
\item Group pattern synonyms according to type. Name one function per type
and put all name-type pairs into context.
\item Define the named functions in context by suffixing pattern synonyms
matching their types. Inject functions from context before variables to
make the function typecheck. For example, if |(x :: A)| occurs on
left-hand-side of a pattern synonym and |(x :: N)| occurs on
right-hand-side, then prefix |x| in the body of the function
by the name associated with the type |A -> N| in the context.
\end{enumerate}

Let us carry out these steps on the transformation from $C$ to $A$.
\begin{enumerate}
\item Put |id| into context. Now the context is:

< Gamma = id :: forall a. a -> a.

\item Group pattern synonyms by types. We obtain the following types:
\begin{align*}
&& C \R A && S \R A && F \R A.
\end{align*}
Call these functions |c2a|, |s2a| and |f2a|. The context becomes:

< Gamma = id :: forall a. a -> a, c2a :: C -> A, s2a :: S -> A, f2a :: F -> A.

\item Generate definitions for the functions |c2a|, |s2a| and |f2a|,
inserting conversions from the context as necessary.
\end{enumerate}
At the end, we obtain the following definitions.

\begin{code}
c2a :: C -> A
c2a (C1 x Add y)  =  A1 (c2a x) Add (s2a y)
c2a (C2 x)        =  s2a x

s2a :: S -> A
s2a (S3 x Mul y)  =  A3 (s2a x) Mul (f2a y)
s2a (S4 x)        =  f2a x

f2a :: F -> A
f2a (F5 n)        =  A5 (id n)
f2a (F6 LP x RP)  =  c2a x
\end{code}

\bigbreak
As a second example, let's convert $A$ to $N$.

\begin{code}
a2n :: A -> N
a2n (A1 x Add y)  =  N1 (a2n x) (P1 Add (a2n y))
a2n (A3 x Mul y)  =  N3 (a2n x) (Q3 Mul (a2n y))
a2n (A5 n)        =  N5 (id n)
\end{code}

\subsection{Backward transformation}

Linear pattern synonyms are invertible; we just have to swap their
left-hand-side and right-hand-side. Swapping the pattern synonyms
between $C$ and $A$ give us the following synonyms:


< pattern A1 (x :: A) Add (y :: A)  :: A  {--}  = {--}  C1 (x :: C) Add (y :: S)  :: C
< pattern x :: A                    :: A  {--}  = {--}  C2 (x :: S)               :: C
< pattern A3 (x :: A) Mul (y :: A)  :: A  {--}  = {--}  S3 (x :: S) Mul (y :: F)  :: S
< pattern x :: A                    :: A  {--}  = {--}  S4 (x :: F)               :: S
< pattern A5 (n :: Int)             :: A  {--}  = {--}  F5 (n :: Int)             :: F
< pattern x :: A                    :: A  {--}  = {--}  F6 LP (x :: C) RP         :: F


Compiling the backward pattern synonyms produce the backward
transformations from $A$ to $C$.

\begin{code}
a2c :: A -> C
a2c (A1 x Add y)  =  C1 (a2c x) Add (a2s y)
a2c x             =  C2 (a2s x)

a2s :: A -> S
a2s (A3 x Mul y)  =  S3 (a2s x) Mul (a2f y)
a2s x             =  S4 (a2f x)

a2f :: A -> F
a2f (A5 n)        =  F5 (id n)
a2f x             =  F6 LP (a2c x) RP
\end{code}

Note that the pattern synonyms with |C2|, |S4| and |F6| come after
the other patterns. That is because the left-hand-side of these
synonyms are more general than the left-hand-side of other synonyms
of the same type. We implement the strategy that if a pattern is
more specific than other patterns, then it is tested first in the
syntax tree transformation.

\subsection{Sorting patterns by specificity}

In order to decide which pattern comes first in a syntax tree
transformation, we have to compare patterns to see which one
is more specific. First-order unification helps us compare
patterns for specificity.

Two patterns $p,q$ are \textbf{unifiable} if there exists a
substitution $\sigma$ such that $\sigma(p)=\sigma(q)$, in which
case $\sigma$ is called a \textbf{unifier} of $p,q$. Non-unifiable
patterns are said to be \textbf{disjoint}.

A unifier $\sigma'$ of $p,q$ is the \textbf{most general unifier}
if for all unifier $\sigma$ of $p,q$, there exists a substitution
$\sigma_2$ such that $\sigma = \sigma_2 \circ \sigma'$. It is known
that all unifiable pairs of patterns have most general unifiers.

The \textbf{intersection} of patterns $p$ and $q$, written
$p\cap q$, is
\begin{itemize}
\item the result
$\sigma(p)=\sigma(q)$ with respect to a most general unifier
if $p$ and $q$ are unifiable, and
\item empty if $p$ and $q$ are disjoint.
\end{itemize}

A pattern $p$ is \textbf{more specific} than another pattern $q$,
written $p <: q$, if
\begin{itemize}
\item $p$ and $q$ are unifiable, and
\item the intersection of $p\cap q$ is $\alpha$-equivalent
to $p$.
\end{itemize}
If neither $p <: q$ nor $q <: p$, then $p$ and $q$ are
\textbf{incomparable}.

Given a set of pattern synonyms of the same type, we require
their left-hand-side to form disjoint classes of patterns
linearly ordered by specificity. If this is not the case,
then there exist incomparable unifiable patterns $p,q$ with
nonempty intersection. In this case we require all
unifier $\sigma$ of $p,q$ to also unify the right-hand-side
of the pattern synonyms where $p$ and $q$ occur.


\section{Nonlinear patterns}

Consider desugaring |int a, b, c;| into |int a; int b; int c;|.

The sugared grammar $G$ describes declaration sequences with
lists of names.
\begin{align*}
G & \R D~G & (G_1)\\
G & \R \epsilon & (G_2)\\
D & \R T~U~; & (D_3)\\
U & \R V~,~U & (U_4)\\
U & \R V & (U_5)\\
T & \R \mbox{string} & (T_5) \\
V & \R \mbox{string} & (V_6)
\end{align*}

The desugared grammar $G'$ describes sequences of declarations
with one name.
\begin{align*}
G' & \R D'~G' & (G_1')\\
G' & \R \epsilon & (G_2')\\
D' & \R T~V~; & (D_3')\\
\end{align*}

Let us convert $G$ and $G'$ into datatypes. For simplicity, we
leave out the singleton types for the semicolon and comma string
literals.

\begin{code}
data G   =  G1 D G
         |  G2

data G'  =  G1' D' G'
         |  G2'

data D   =  D3 T U

data D'  =  D3' T V

data T   =  T4 String

data U   =  U5 V U
         |  U6 V

data V   =  V7 String
\end{code}

The desugaring transformation operates as follows. Note that some
recursive call to |desugar| is not structurally recursive.

\begin{code}
desugar :: G -> G'
desugar (G1 (D3 t (U5 v u)) g)  =  G1' (D3' t v) (desugar (G1 (D3 t u) g))
desugar (G1 (D3 t (U6 v)) g)    =  G1' (D3' t v) (desugar g)
desugar G2                      =  G2'
\end{code}

Non-structural-recursion are expressed by \emph{recursive} pattern
synonyms, where constructors on left-hand-side may occur on
right-hand-side. This is the recursive pattern synonym from |G|
to |G'|:

< pattern G1 (D3 t (U5 v u)) g  =  G1' (D3' t v) (G1 (D3 t u) g)
< pattern G1 (D3 t (U6 v)) g    =  G1' (D3' t v) g
< pattern G2                    =  G2'

\end{document}
