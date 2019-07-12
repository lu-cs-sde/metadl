package org.extendj.scanner;

import beaver.Symbol;
import beaver.Scanner;
import org.extendj.parser.JavaParser.Terminals;
import java.io.*;
%%

%public
%final
%class JavaScanner
%extends Scanner

%type Symbol
%function nextToken
%yylexthrow Scanner.Exception

%unicode
%line %column
