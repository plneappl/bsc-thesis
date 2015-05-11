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


\usepackage{cleveref,pbox,amsmath,stmaryrd}
\usepackage{caption,subcaption,float}

% for "Bringert and Ranta [1]"
\usepackage[numbers]{natbib}
\bibliographystyle{abbrvnat}

% Refer to footnotes via \footnotemark.
% http://tex.stackexchange.com/a/10116/1340
\crefformat{footnote}{#2\footnotemark[#1]#3}

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
\subtitle{Bidirectional Grammar/Datatype Transformation\\
Via Recursive Pattern Synonyms with Functor Literals}


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

\section{Introduction}

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

\begin{code}
data S  =  S_1 S Plus F
        |  S_2 F

data F  =  F_3 Int

data Plus = Plus -- the terminal symbol +
\end{code}

The datatype declaration fully describes the language of
left-associative sums, which is an LL(1) language. The user
should be able to ``derive'' an LL(1) parser simply by requesting
it in the datatype declarations.

< data S  =  S_1 S Plus F  |  S_2 F  {--}  deriving LL1
< data F  =  F_3 Int                 {--}  deriving LL1

< expr = parseLL1 "1 + 2 + 3"
< -- |S_1  (S_1 (S_2 (F_3 1)) Plus (S_2 (F_3 2))) Plus (S_2 (F_3 3))|


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

\item Rapid deployment of parsing technologies: New technologies
implemented for datatypes-as-language-descriptions have a low
adoption overhead, because users can call them as a library
instead of learning the technique and applying it to their
language manually. Together with the widespread support of
algebraic datatypes and the low learning curve, new technologies
can quickly reach a large audience.
\end{enumerate}


The key to achieving this rosy vision is bidirectional
grammar/datatype transformation. To see how it fits the picture,
let us look again at the example of associative sums and think
about the implementation of the |LL1|-derivation mechanism.
Note that the grammar $S$ is left-recursive. In order to develop
a LL(1) parse table for it, we must first eliminate left
recursion by a textbook procedure to produce a grammar $S'$.

\begin{align*}
S' &::= F~R & (S_1')\\
R  &::= +~F & (R_1)\\
R  &::= \epsilon & (R_2)\\
F  &::= \mbox{integer} & (F_3)
\end{align*}

Following \cref{gdc}, we can define new datatypes for the syntax
trees of $S'$ and build an LL(1) parser to produce values of the
datatype |S'|.

\begin{code}
data S'  =  S_1prime F R
data R   =  R_1 Plus S'  |  R_2
\end{code}

However: The grammar $S'$ is created to appease the LL(1) parsing
algorithm. To maintain the abstraction barrier, the derived LL(1)
parser should produce the abstract syntax trees |S| instead. The
missing link is the coercion from |S'| to |S|, without which
left-recursion-elimination cannot be an automatic grammar
transformation.

We have seen how grammar transformations are important to support
datatypes as language descriptions. Why, then, do we want
\emph{bidirectional} grammar transformations? The reason is
twofold: that we \emph{can} support it, and that it's useful
sometimes.
\begin{enumerate}
\item \emph{Feasibility}: The grammar transformations are
intended to be information-preserving in a weak sense; by parsing
into a different grammar, we intend to stay true to the original
grammar, neither creating nor eliminating information. A backward
syntax tree transformation communicates our intention of
information preservation.
\item \emph{Utility}: Pretty-printing from abstract syntax tree
is useful sometimes. Example use cases include the interpreter
REPL and code generation in metaprogramming.
\end{enumerate}



\section{Our approach and its relation to existing solutions}

The rough idea is to describe bidirectional syntax tree
transformation by recursive pattern synonyms with functor
literals, which are generated by grammar transformations acting
on a particular grammar. Here comes a design question:
\begin{quotation}
Can one unify functors and pattern synonyms?
\end{quotation}

\noindent Here are some example usages.

\medbreak
< pattern S_1prime f_1 (F (S_2 f_2))  =  F (S_1 (S_2 f_1) f_2)
<
< functor F s  =  s @ (S_2 f)
< functor F s  =  S_1 (F s) Plus f
\medbreak

\noindent Alternatively:

\medbreak
< pattern S_1prime f_1 r@(G f_2)  =  r@(F (S_1 (S_2 f_1) f_2))
<
< functor G f = F (S_2 f)
\medbreak


\cite{Lae01}
\bibliography{master}

\end{document}
