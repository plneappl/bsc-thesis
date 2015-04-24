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



\gdef\R{\rightarrow}

\newtheorem{lemma}[subsection]{Lemma}

\begin{document}

\null
\vskip 3cm plus 1cm
\title{Syntax tree transformation as dialgebraic fold}
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
data Add  =  Add  -- +
data Mul  =  Mul  -- $\times$
data LP   =  LP   -- (
data RP   =  RP   -- )
\end{code}

\PAR
\begin{code}
-- The concrete grammar $C$

data C  =  C1 C Add S
        |  C2 S

data S  =  S3 S Mul F
        |  S4 F

data F  =  F5 Int
        |  F6 LP C RP
\end{code}

\PAR
\begin{samepage}
\begin{code}
-- The abstract grammar $A$

data A  =  A1 A Add A
        |  A3 A Mul A
        |  A5 Int
\end{code}
\end{samepage}

\PAR
\begin{code}
-- The grammar $N$ in Chomsky 2-normal form

data N  =  N1 N P
        |  N3 N Q
        |  N5 Int

data P  =  P1 Add N

data Q  =  Q3 Mul N
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

\end{document}
