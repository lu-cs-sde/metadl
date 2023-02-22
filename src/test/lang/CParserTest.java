package lang;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.StringReader;
import org.apache.commons.lang3.tuple.Pair;
import beaver.Symbol;
import beaver.Scanner;
import se.lth.sep.Category;
import se.lth.sep.Util;
import se.lth.sep.ParseTree;
import se.lth.sep.Grammar;
import se.lth.sep.EarleyParser;
import se.lth.sep.SPPFNode;
import se.lth.sep.SPPFTrivialProductionRemover;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;


public class CParserTest {
	public static java.util.List<Symbol> scan(String s) {
		return CTestUtil.scan(new lang.c.obj.ast.ObjLangScanner(new StringReader(s)));
	}

	public static java.util.List<ParseTree> parse(java.util.List<Symbol> tokens, Category startSymbol) {

		String[] tokenNames = lang.c.obj.ast.ObjLangParserSEP.Terminals.NAMES;
		var astBuilder = lang.c.obj.ast.ASTBuilder.getInstance();
		java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, startSymbol, astBuilder, tokenNames);
		return parseTrees;
	}

	@Test
	public void testStructAndUnionDeclaration() {
		String c = "struct f { union { int x; float f; } un; int bitfield : 10; } foo(int z);";
		Category start = lang.c.obj.ast.ObjLangParserSEP.n_declaration;

		java.util.List<ParseTree> parseTrees =  parse(scan(c), start);
		assertNotNull(parseTrees);

		Util.dumpParseTrees("test1_", parseTrees);
	}

	@Test
	public void testArrayDeclaration() {
		String c = "int x[10];";
		Category start = lang.c.obj.ast.ObjLangParserSEP.n_declaration;

		java.util.List<ParseTree> parseTrees =  parse(scan(c), start);
		assertNotNull(parseTrees);

		Util.dumpParseTrees("test2_", parseTrees);
	}

	@Test
	public void testFunctionPtrDeclaration() {
		String c = "int (*f(int, int))(float);";

		for (int i = 0; i < 3; ++i) {
			c = c + c;
		}

		Category start = lang.c.obj.ast.ObjLangParserSEP.n_translation_unit;

		java.util.List<Symbol> tokens = scan(c);
		java.util.List<ParseTree> parseTrees =  parse(tokens, start);
		assertNotNull(parseTrees);

		// Util.dumpParseTrees("test3_", parseTrees);

		CTestUtil.<lang.c.obj.ast.ASTNode>buildAST(tokens, parseTrees,
												   lang.c.obj.ast.ASTBuilder.getInstance());
	}

	@Test
	public void testTranslationUnit1() {
		String c = "struct { int a; union { float x; } u; } my_struct;";
		Category start = lang.c.obj.ast.ObjLangParserSEP.n_translation_unit;

		java.util.List<Symbol> tokens = scan(c);
		java.util.List<ParseTree> parseTrees = parse(tokens, start);

		assertNotNull(parseTrees);

		for (lang.c.obj.ast.ASTNode ast : CTestUtil.<lang.c.obj.ast.ASTNode>buildAST(tokens, parseTrees,
																					 lang.c.obj.ast.ASTBuilder.getInstance())) {
			ast.debugPrint(System.out);
		}
	}

	private static void testAST(String c) {
		testAST(c, lang.c.obj.ast.ObjLangParserSEP.n_translation_unit);
	}

	private static void testAST(String c, Category start) {
		System.out.println("================================================================================");
		System.out.println("Parsing: \"" + c + "\"");
		System.out.println("================================================================================");

		java.util.List<Symbol> tokens = scan(c);
		java.util.List<ParseTree> parseTrees = parse(tokens, start);

		assertNotNull(parseTrees);

		Util.dumpParseTrees("test3_", parseTrees);

		for (lang.c.obj.ast.ASTNode ast : CTestUtil.<lang.c.obj.ast.ASTNode>buildAST(tokens, parseTrees,
																					 lang.c.obj.ast.ASTBuilder.getInstance())) {
			ast.debugPrint(System.out);
		}
	}

	@Test
	public void testMultipleDecls() {
		String c =
			"\ntypedef extern const volatile VFLOAT;" +
			"\nextern const volatile float x, *y;" +
			"\nVFLOAT z, *w;";

		for (int i = 0; i < 3; ++i) {
			c = c + c;
		}

		testAST(c);
	}

	@Test
	public void testSingleDecl() {
		testAST("int x = 10;");
	}

	@Test
	public void testTypedef() {
		testAST("typedef struct U {int x; int y; } U;");
	}

	@Test
	public void testFunctionDeclaration() {
		testAST("int foo(void);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testExpression() {
		testAST("foo(x + 1) + z[1][sizeof(int)]->m.n", lang.c.obj.ast.ObjLangParserSEP.n_expression);
	}

	@Test
	public void testEmptyBlock() {
		testAST("{}", lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testCallStatement() {
		testAST("printf(\"Hello world!\");", lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testDoWhileStatement() {
		testAST("do { printf(\"Hello world!\"); } while(cond);", lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testForStatement() {
		testAST("for (;;) ;", lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testForDeclStatement() {
		testAST("for (int n; ; ) ;", lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testComplexStatement() {
		testAST("for (i = 1; i < 10; ++i) {" +
				"  do {" +
                     "printf(\"Hello world\");" +
				"  } while (1); " +
				"}",
				lang.c.obj.ast.ObjLangParserSEP.n_statement);
	}

	@Test
	public void testFunctionDefinition() {
		testAST("int f(int x, int y) { return x + y; }", lang.c.obj.ast.ObjLangParserSEP.n_function_definition);
	}

	@Test
	public void testFunctionDefinitions() {
		testAST("int f(int x, int y) { return x + y; }" +
				"int (*g(void))(int, int) { return &f; }");
	}

	@Test
	public void testFunctionPtrType1() {
		testAST("int (****p)(int);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testFunctionPtrType2() {
		testAST("int ((****p)(int));", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testFunctionPtrType3() {
		testAST("int ((****p)(int (**)(int)));", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testAbstractDeclarator1() {
		testAST("(*)(int (*)(int))", lang.c.obj.ast.ObjLangParserSEP.n_abstract_declarator);
	}

	@Test
	public void testAbstractDeclarator2() {
		testAST("(*)(int)", lang.c.obj.ast.ObjLangParserSEP.n_abstract_declarator);
	}

	@Test
	public void testAbstractDeclarator3() {
		testAST("*", lang.c.obj.ast.ObjLangParserSEP.n_abstract_declarator);
	}

	@Test
	public void testAbstractDeclarator4() {
		testAST("(*)", lang.c.obj.ast.ObjLangParserSEP.n_direct_abstract_declarator);
	}

	@Test
	public void testAbstractDeclarator5() {
		testAST("(*)", lang.c.obj.ast.ObjLangParserSEP.n_abstract_declarator);
	}


	@Test
	public void testFunctionReturningPtr() {
		testAST("int *p(int);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}


	@Test
	public void testPtrToFunction() {
		testAST("int (*f)(int);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testTwoPtrToFunction1() {
		testAST("int (**f)(int);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testTwoPtrToFunction2() {
		testAST("int (*(*f))(int);", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testHigherOrderFunc() {
		testAST("int f(int (int, int));", lang.c.obj.ast.ObjLangParserSEP.n_declaration);
	}

	@Test
	public void testIf1() {
		testAST("if (x > y) if (x == z) x++; else x--;", lang.c.obj.ast.ObjLangParserSEP.n_selection_statement);
	}

	@Test
	public void testIf2() {
		testAST("if (x > y) { if (x == z) x++; else x--; }", lang.c.obj.ast.ObjLangParserSEP.n_selection_statement);
	}

}
