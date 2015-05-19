% Hey Emacs, this is -*- latex -*-
\documentclass[preprint]{sigplanconf}

\newcommand\KEYWORD[1]{\textbf{#1}}
%include lhs2TeX.fmt
%lang haskell
%include DirtyWork.tex
%subst nested a = "\textrm{ " a " }"

% The following \documentclass options may be useful:

% preprint      Remove this option only once the paper is in final form.
% 10pt          To set in 10-point type instead of 9-point.
% 11pt          To set in 11-point type instead of 9-point.
% authoryear    To obtain author/year citation style instead of numeric.

\usepackage{cleveref,pbox,amsmath,stmaryrd,verbatim}
\usepackage{caption,subcaption,float}
\usepackage{tikz}
\usetikzlibrary{shapes,arrows,decorations.markings}
\tikzstyle{block} = [rectangle, draw,
    text width=5em, text centered, rounded corners, minimum
    height=4em]
\tikzstyle{vecArrow} = [thick, decoration={markings,mark=at position
   1 with {\arrow[thick]{open triangle 90}}},
   double distance=3pt, shorten >= 4.5pt,
   preaction = {decorate},
   postaction = {draw,line width=3pt, white,shorten >= 2.5pt}]
\tikzstyle{innerWhite} = [semithick, white,line width=3pt, shorten >= 4.5pt]


% for "Bringert and Ranta [1]"
\usepackage[numbers]{natbib}
\bibliographystyle{abbrvnat}

% Refer to footnotes via \footnotemark.
% http://tex.stackexchange.com/a/10116/1340
\crefformat{footnote}{#2\footnotemark[#1]#3}

\begin{comment}
\begin{code}
import Control.Exception(assert)
\end{code}
\end{comment}

\begin{document}

\special{papersize=8.5in,11in}
\setlength{\pdfpageheight}{\paperheight}
\setlength{\pdfpagewidth}{\paperwidth}

\conferenceinfo{CONF 'yy}{Month d--d, 20yy, City, ST, Country}
\copyrightyear{20yy}
\copyrightdata{978-1-nnnn-nnnn-n/yy/mm}
\doi{nnnnnnn.nnnnnnn}

% Uncomment one of the following two, if you are not going for the
% traditional copyright transfer agreement.

%\exclusivelicense                % ACM gets exclusive license to publish,
                                  % you retain copyright

%\permissiontopublish             % ACM gets nonexclusive license to publish
                                  % (paid open-access papers,
                                  % short abstracts)

%\titlebanner{banner above paper title}        % These are ignored unless
%\preprintfooter{short description of paper}   % 'preprint' option specified.

\title{Datatypes as Language Descriptions}
\subtitle{Bidirectional Grammar/Datatype Transformation
via Recursive Pattern Synonyms}


\authorinfo{One\and Two\and Three}
{University of T\"ubingen, Germany}
{}

\maketitle

\begin{abstract}
\end{abstract}

% Don't need to fill in before acceptance.
%\category{CR-number}{subcategory}{third-level}

% general terms are not compulsory anymore,
% you may leave them out
%\terms
%term1, term2

% TENTATIVE: here for the sake of example
\keywords Bidirectional transformations, pattern synonyms,
rewrite rules

\section{Introduction}\label{intro}

Bacchus-Naur Form is a popular format of describing context-free
grammars. Algebraic datatypes are an important
feature in modern funcitonal programming languages. The syntax of
Bacchus-Naur Form is almost identical to the declaration of
mutually recursive regular datatypes. This document explores how
far this connection can be exploited in language engineering. We
envision a framework in which compilers and interpreters are
written with neither parser generators nor parser combinators,
where the datatype of syntax trees is sufficient description of
the parser---\emph{and} the pretty printer.

Let us walk through the idea with a simple example:
left-associatve sum expressions. This is the grammar $S$ with
named production rules:

\begin{align*}
S &::= S~+~F & (S_1)\\
S &::= F & (S_2)\\
F &::= \mbox{integer} &(F_3)
\end{align*}

Following the grammar-datatype correspondance schema in
\cref{gdc}, we transform the grammar $S$ into datatypes |S| and
|F|. The syntax trees of left-associative sums are exactly the
values of |S|.

< data S  =  S_1 S Plus F
<         |  S_2 F
<
< data F  =  F_3 Int
<
< data Plus = Plus -- the terminal symbol +

\begin{comment}
\begin{code}
data S  =  S_1 S Plus F
        |  S_2 F deriving (Show, Eq)

data F  =  F_3 Int deriving (Show, Eq)

data Plus = Plus deriving (Show, Eq)
\end{code}
\end{comment}

The datatype declaration fully describes the language of
left-associative sums, which is an LL(1) language. The user
should be able to ``derive'' an LL(1) parser simply by requesting
it in the datatype declarations.

< data S  =  S_1 S Plus F  |  S_2 F  {--}  deriving LL1
< data F  =  F_3 Int                 {--}  deriving LL1

< expr = parseLL1 "1 + 2 + 3"
< -- |S_1  (S_1 (S_2 (F_3 1)) Plus (F_3 2)) Plus (F_3 3)|

\begin{comment}
\begin{code}
expr = S_1  (S_1 (S_2 (F_3 1)) Plus (F_3 2)) Plus (F_3 3)
\end{code}
\end{comment}


\begin{figure}
\centering
\begin{tabular}{rcl}
Grammar & is & family of mutually recursive datatypes \\
Nonterminal symbol & is & datatype \\
Terminal symbol & is & primitive type or singleton type \\
Production rule & is & data constructor \\
Syntax tree & is & value of a datatype
\end{tabular}
\caption{Grammar-datatype correspondance.}
\label{gdc}
\end{figure}

There are several advantages in specifying a langauge's concrete
syntax through the datatype of its abstract syntax trees.
\begin{enumerate}
\item Rapid prototyping: The delay between the design and
implementation of a programming language is shortened. Writers of
a prototype interpreter can stop worrying about concrete syntax
without risking the cumbersome manipulation of syntax trees
obscuring their language's characteristics.

\item Shallow learning curve: Some basic computer science
education and \cref{gdc} are all that is required to use
datatypes as language descriptions. Users can treat parsing as a
black box. They need only be aware of the \emph{external}
characteristics of a parsing algorithm to request it for their
syntax tree datatype; detailed knowledge about particular parser
generators or parser combinator libraries are unnecessary.

\item Reuse: Standard parsing technology can be implemented once
and for all; users need not re-invent the wheel for their own
languages. For example, left-recursion-elimination is necessary
to generate an LL(1) parser for the language of left-associative
sums. With left-recursion-elimination as a component of the
|LL1|-derivation mechanism, users need not carry out the process
manually for each of their languages.

\item Rapid deployment of parsing technologies: Once a new
technology (like mixfix parsing~\citep{Dan11}) is
implemented for datatypes-as-language-descriptions, it enjoys a
low adoption overhead. Instead of learning the technique and
applying it to their language manually, users can invoke the
tool in a library. Together with the widespread support of
algebraic datatypes and the low learning curve, new technologies
can quickly reach a large audience.
\end{enumerate}


The key to achieving this rosy vision is bidirectional
grammar/datatype transformation. To see how it fits the picture,
let us look again at the example of left-associative sums and think
about the implementation of the |LL1|-derivation mechanism.
Note that the grammar $S$ is left-recursive. In order to develop
an LL(1) parse table for it, we must first eliminate left
recursion by a textbook procedure, producing the grammar $S'$.

\begin{align*}
S' &::= F~R & (S_1')\\
R  &::= +~S' & (R_1)\\
R  &::= \epsilon & (R_2)\\
F  &::= \mbox{integer} & (F_3)
\end{align*}

Following \cref{gdc}, we can define new datatypes for the syntax
trees of $S'$ and build an LL(1) parser to produce values of the
datatype |S'|.

< data S'  =  S_1prime F R
< data R   =  R_1 Plus S'  |  R_2

\begin{comment}
\begin{code}
data S'  =  S_1prime F R deriving (Show, Eq)
data R   =  R_1 Plus S'  |  R_2 deriving (Show, Eq)
\end{code}
\end{comment}

However: The grammar $S'$ is created to appease the LL(1) parsing
algorithm. To maintain the abstraction barrier, the derived LL(1)
parser should produce values of the datatype |S| instead. The
missing link is the coercion from |S'| to |S|, without which
left-recursion-elimination cannot be fully automatic.
We will show the code of the coercion later
[CROSSREF].

We have seen how grammar transformations are important to support
datatypes as language descriptions. Why, then, do we want
\emph{bidirectional} grammar transformations? Firstly, the
backward transformation is useful in pretty-printing: Since the
concrete syntax tree is closer to the string syntax, it is easier
to convert an abstract syntax tree to a concrete syntax tree
before printing it. Secondly, the backward transformation is used
in the forward transformation sometimes [CROSSREF].


\section{Our approach, built from existing solutions}

\subsection{Architecture and grammar adaptation}

\Citeauthor{LaeFE} perceived that transformations at the grammar
level induce transformations at the syntax tree level
\citep{LaeFE,LaeGA}. To exploit the connection, they defined a
collection of grammar-level operators, each of which induces an
operator at the syntax-tree level. Grammar and syntax tree
transformations are described at the same time by composing these
operators. Their architecture is summarized in \cref{lae}; it is
the starting point of our proposal.

One weakness in \citeauthor{LaeFE}'s architecture is the one-one
correspondance between grammar transformations and syntax tree
transformations. Under this framework, every grammar operator has
to make sense as a syntax tree transformation for \emph{all
possible grammars}. It is hard, if not impossible, to support a
different syntax tree transformation for each input grammar.
Left-recursion-elimination is one of the difficult cases. On
grammars without left recursion, its syntax tree transformation
is identity. On grammars with left recursion, its syntax tree
transformation is \emph{not} the identity, even on trees that do
not exhibit left recursion themselves. To support such
transformations, we refine \citeauthor{LaeFE}'s architecture to
associate each grammar transformation with a \emph{function} from
grammars to syntax tree transformations, described by some
intermediate language (\cref{arch}).

\begin{figure}
\centering
\begin{tikzpicture}[y=1cm,x=2cm]
\path
(0,2)node [block] (ing) {input grammar}
(2,2)node [block] (oug) {output grammar}
(0,0)node [block] (int) {input syntax tree}
(2,0)node [block] (out) {output syntax tree}
;
\draw[->](ing)--(oug)
node[pos=.5,anchor=north](ada){``adaptation''}
;
\draw[->](int)--(out)
node[pos=.5,anchor=south](mig){``migration''}
;
\draw[vecArrow](ada)--(mig);
\end{tikzpicture}
\caption{Grammar transformation architecture by
\citeauthor{LaeFE} \citep{LaeFE,LaeGA}, with ``document type
definition'' replaced by ``grammar'' and ``XML document''
replaced by ``syntax tree''.}
\label{lae}
\end{figure}

\begin{figure}
\centering
\begin{tikzpicture}[x=2cm,y=2cm]
\tikzstyle{lang} = [block,draw=none,text width=7em]
\path
(0.0,1.5)node[block] (ing) {input grammar}
(.95,2.5)node[lang]  (gtl) {grammar transformation language}
(1,0,0.0)node[block] (int) {input syntax tree}
(2.0,2.0)node[block] (oug) {output grammar}
(2.0,1.0)node[lang]  (med) {intermediate language}
(3.0,0.0)node[block] (out) {output syntax tree}
;
\draw[->](ing)--+(0:1.4)node[pos=1.0,anchor=south east](ada){``adaptation''}--(oug);
\draw[->](ing)  +(0:1.4)--(med);
\draw[vecArrow](gtl)--+(-90:0.7);
\draw[vecArrow](med)--+(-90:0.6);
%
\draw[transform canvas={yshift=1mm},->](int)--(out)
node[pos=.5,anchor=south](mig){``migration''};
\draw[transform canvas={yshift=-1mm},->](out)--(int)
node[pos=.5,anchor=north](mig){``repatriation''};
\end{tikzpicture}
\caption{Refined architecture of grammar and syntax tree
transformations. We choose \emph{recursive pattern synonyms} as
the intermediate language.}
\label{arch}
\end{figure}

\subsection{Pattern synonyms and views}\label{views}

The choice of the intermediate language in \cref{arch}
is a key design decision.
It has to be powerful enough to support common and established
grammar transformations in language engineering, it has to be
regular enough to be generated automatically, and it has to be
bidirectional enough to support backward syntax tree
transformations. \emph{Recursive pattern synonyms} fulfills all 3
conditions with the additional benefit of being intuitive to
human programmers, who then retains the possibility to code in
the intermediate language.

Pattern synonyms are ``macro definitions'' for the patterns in
pattern-matching. They were invented as a form of data
abstraction for algebraic datatypes. For example, one may write a
pattern synonym |Triple| to pattern-match on lists of 3 elements:

< pattern Triple a b c = a : b : c : []


There is a deep connection between pattern synonyms and
\emph{views}~\citep{Wad87}. In fact, one may think of views as a
way to implement of many kinds of pattern synonyms. From the
\emph{views} perspective, constructors defined by pattern
synonyms constitute a datatype of their own, called the
\emph{view}. In the example above, |Triple| would be a
constructor of a view on lists. There are bidirectional
conversions between the original datatype and views, which are
invoked every time the user pattern-matches on the original datatype
with a view constructor. Our approach shares with \emph{views}
the mental picture of pattern matching as bidirectional
conversion, but our objective is the ``inverse'' of that of
views: Bidirectional conversions are our desired output, and we
support them through pattern synonyms.

Our interest is about \emph{recursive pattern synonyms}, or more
precisely, nonlinear, nested, mutually-recursive pattern
synonyms.
\begin{itemize}
\item \emph{Nonlinearity}: the same variable may occur multiple
times on both sides of a pattern synonym.
\item \emph{Nesting}: The pattern on left-hand-side of a synonym
may contain multiple constructor names.
\item \emph{Mutual-recursion}: Constructor names on
left-hand-side of a synonym may occur on right-hand-side of
the same or a different synonym. Vice versa for constructors on
right-hand-side of synonyms. Constructors for different types may
freely intermix.
\end{itemize}
Nonlinear, nested, mutually-recursive pattern synonyms are
powerful enough to express highly nontrivial syntax tree
transformations. For example, left-recursion-elimination on the
grammar of left-associative sums are mostly captured by the
following synonym (\cref{intro}):

< pattern S_1prime f_1 (S_1 s Plus f_n)  =  S_1 (S_1prime f_1 s) Plus f_n

\subsection{Pattern synonyms and rewrite rules}

Pattern synonyms are closely related to rewrite
rules~\citep{Lae03}. In the field of attribute
grammars, the work by \citet{Mar14} is closest to our idea of
compiling pattern synonyms to syntax tree transformations. In
\citeauthor{Mar14}'s system, syntax tree transformations are
described by recursive rewrite rules, which can be inverted to
generate the backward transformation. If we erase all occurrences
of names of rewrite rule from \citeauthor{Mar14}'s system, then
we essentially obtain recursive pattern synonyms, with one
important restriction: The left-hand-side of these synonyms may
only contain constructors of the source grammar. In particular,
the synonym for left-recursion-elimination at the end of
\cref{views} is forbidden. This restriction limits the
expressiveness of syntax tree transformations, forbidding
classical use cases like left-recursion elimination. Moreover, it
destroys the closure property of inversion: Backward
transformations have to be written in a language bigger than the
language of forward transformations, and cannot be inverted again
with the same technique. In this sense, recursive pattern
synonyms are the natural \emph{closure} of \citeauthor{Mar14}'s
rewrite rules, gaining symmetry, robustness and conceptual
clarity in the process.

\section{Recursive pattern synonyms}

This section describes the syntax of recursive pattern synonyms.
\Cref{semantics} describes their meanings. For a pattern synonym
to be meaningful as bidirectional tree transformation, we require
only that top-level variables are type-annotated.
\begin{align*}
s & ::= \textbf{pattern}~p~=~p &
\mbox{pattern syonym}
\\\\
p & ::=  &\mbox{pattern}\\
&\quad\mid~ x & \mbox{variable}\\
&\quad\mid~ c~\overline{p} &\mbox{constructor application}\\
&\quad\mid~ p :: \tau &\mbox{type annotation}
\end{align*}

As an example, here are all the pattern synonyms necessary to
describe the tree transformation from the right-recursive grammar
$S'$ to the left-recursive grammar $S$ (\cref{intro}). The
right-hand-side of the last pattern synonym is the variable |s|
annotated by the data\-type~|S|.


< pattern S_1prime f_1 R_2               =  S_2 f_1
< pattern S_1prime f_1 (S_2 f_2)         =  S_1 (S_2 f_1) Plus f_2
< pattern S_1prime f_1 (S_1 s Plus f_n)  =  S_1 (S_1prime f_1 s) Plus f_n
<
<
< pattern R_1 Plus s                     =  s :: S


\section{Interpreting recursive pattern synonyms}
\label{semantics}

For now, this section gives a rough idea about the meaning of
recursive pattern synonyms via an inaccurate translation
procedure into Haskell. To make the translation precise, we need
a deeper understanding of recursive pattern matching
(\cref{meta}).

%format fromS = "\ensuremath{\Varid{from}_{S^\prime}}"
%format toS   = "\ensuremath{\Varid{to}_{S^\prime}}"
%format fromR = "\ensuremath{\Varid{from}_{R}}"
%format toR   = "\ensuremath{\Varid{to}_{R}}"

\begin{code}
fromS  ::  S' -> S
toS    ::  S -> S'
fromR  ::  R -> S
toR    ::  S -> R

fromS  (S_1prime f_1 R_2)  =   S_2 f_1

fromS  (S_1prime f_1 r)    =   case fromR r of
       S_2 f_2             ->  S_1 (S_2 f_1) Plus f_2
       S_1 s Plus f_n      ->  S_1 (fromS (S_1prime f_1 (toR s))) Plus f_n


fromR (R_1 Plus s') = fromS s'



toR s = R_1 Plus (toS s)

toS  (S_2 f_1)                 =   S_1prime f_1 R_2
toS  (S_1 (S_2 f_1) Plus f_2)  =   S_1prime f_1 (toR (S_2 f_2))
toS  (S_1 s_0 Plus f_n)        =   case toS s_0 of
     S_1prime f_1 s {--} -> {--}  S_1prime f_1 (toR (S_1 (fromR s) Plus f_n))




truth   =  expr == (fromS (toS expr))

\end{code}



\section{Metatheory of recursive pattern synonyms}
\label{meta}

\section{Grammar transformation language}

\section{Intermediate language generation}

\section{Related work}

\section{Conclusion}

\bibliography{master}

\end{document}
