package lang.java5.pat.ast;

import beaver.Symbol;
import beaver.Scanner;
import lang.java5.pat.ast.PatLangParserSEP.Terminals;
import java.io.*;
%%

%public
%final
%class PatLangScanner
%extends Scanner

%type Symbol
%function nextToken
%yylexthrow Scanner.Exception

%unicode
%line %column
