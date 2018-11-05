Datalog Implementation for Project in Computer Science at LTH (EDAN70)
========

## Datalog
[Datalog](https://en.wikipedia.org/wiki/Datalog) is a declarative query language. It enables the derivation of relations expressed as Horn-Clauses.
For an introduction to Datalog see e.g. [Slides EDA045F](http://fileadmin.cs.lth.se/cs/Education/EDA045F/2018/web/slides-06.pdf), or the project report (listed below).

## JFlex
[JFlex](http://jflex.de/) is a lexical analyzer generator and is used to generate the lexical analyzer.

## Beaver
[Beaver](http://beaver.sourceforge.net/) is a LALR(1) parser generator. The parser descriptions are written in EBNF-form and a parser is generated.

## JastAdd
[JastAdd](http://jastadd.org/web/) is a meta-compilation system that supports Reference Attribute Grammars (RAGs). It uses the parser generated from Beaver. In addition it takes an AST-description file as input. The AST-description is used to generate and populate the corresponding classes that represent the dynamic AST.

## JUnit
[JUnit](https://junit.org/junit5/) is used for unit testing.

## Building
[Gradle](https://gradle.org/) is used as a build tool. There is additionally a Makefile to summarize certain common build sequences. 
* Package: ***make jar***
* Test: ***make test***

## Project Report
Included in report/ folder as a .tex-file.
