Contents
========

Grammars
--------

* Theoretical basics
* .grammar-file

Motivation
----------

* simpler parsers (recursive descent <--> left recursion, CYK, ...)
* complex grammars, simple syntax trees
* Textbook describes grammar transformations, but doesn't talk about syntax trees, only conserving the language

Goal
------

* Transforming grammars
* Syntax tree transformation 

DSL meaning
-----------

* in/out/seq -> matchers
* assignments, declarations, functions
* pattern synonyms

Case studies
------------

* `concreteToAbstract`
* `eliminateLeftRecursion1/2/3`
* `leftFactoring`
* `inlining`
* `chomsky`

Inner workings
--------------

* grammar internal representation
* matching -> producing -> pattern synonyms -> prolog definitions
* different atoms
* tables
* matching
* producing
* pattern synonyms: automatic/manual
* scopes
* prolog
* time(/space) complexity
