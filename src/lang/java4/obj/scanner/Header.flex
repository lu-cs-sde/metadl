package lang.java4.obj.ast;

import beaver.Symbol;
import beaver.Scanner;
import lang.java4.obj.ast.ObjLangParser.Terminals;
import java.io.*;
%%

%public
%final
%class ObjLangScanner
%extends Scanner

%type Symbol
%function nextToken
%yylexthrow Scanner.Exception

%unicode
%line %column
