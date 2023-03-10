package clang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.StringReader;
import java.util.List;

import org.junit.jupiter.api.Test;

import beaver.Symbol;
import lang.CTestUtil;
import lang.c.pat.ast.ASTNode;
import lang.c.pat.ast.Declaration;
import se.lth.sep.Category;
import se.lth.sep.ParseTree;

public class TypeTest {
	static <T extends ASTNode> T parse(String s, Category startSymbol) {
		java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(s)));

		var astBuilder = lang.c.pat.ast.ASTBuilder.getInstance();
		java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, startSymbol, astBuilder,
				lang.c.pat.ast.PatLangParserSEP.Terminals.NAMES);

		assertNotNull(parseTrees);

		if (parseTrees.size() > 1) {
			se.lth.sep.Util.dumpParseTrees("ambigous_parse_", parseTrees);
		}

		// assertEquals(1, parseTrees.size(), "Expecting an unambigous parse");

		for (ASTNode ast : CTestUtil.<lang.c.pat.ast.ASTNode>buildAST(tokens, parseTrees, astBuilder)) {
			return (T) ast;
		}

		return null;
	}

	@Test
	public void test1() {
		String s = "int *array[10];";
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);
	}

	@Test
	public void test2() {
		String s = "int (*array)[10];";
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);
	}

	@Test
	public void test3() {
		String s = "const int * const p = 10;";
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);
	}

	private static void debugDecl(String s) {
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}

	private static void checkTypes(String decl, String ... expectedTypes) {
		Declaration d = parse(decl, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		List<AST.Decl> result = d.clangDecls();

		//assertEquals(expectedTypes.length, result.size());
		for (int i = 0; i < result.size(); ++i) {
			if (expectedTypes[i] == null)
				continue;
			assertEquals(expectedTypes[i], result.get(i).explicitType.printAsType());
		}
	}


	@Test
	public void test4() {
		String s = "const int * const p = 10, q = 11;";
		debugDecl(s);
	}

	@Test
	public void test5() {
		String s = "int p, q;";
		debugDecl(s);
		checkTypes(s, "(INT)", "(INT)");
	}

	@Test
	public void test6() {
		String s = "const int p[const 10];";
		debugDecl(s);
		checkTypes(s, "([] CONST (INT CONST))");
	}

	@Test
	public void test7() {
		String s = "int foo(int);";
		debugDecl(s);
		checkTypes(s, "(() (INT) (INT))");
	}

	@Test
	public void test8() {
		String s = "int foo(int n);";
		debugDecl(s);
		checkTypes(s, "(() (INT) (INT))");
	}


	@Test
	public void test9() {
		String s = "int (*foo)(int);";
		debugDecl(s);
		checkTypes(s, "(* (() (INT) (INT)))");
	}

	@Test
	public void test10() {
		String s = "int *p[10];";
		debugDecl(s);
		checkTypes(s, "([] (* (INT)))");
	}

	@Test
	public void test11() {
		String s = "int (* const p)[10];";
		debugDecl(s);
		checkTypes(s, "(* CONST ([] (INT)))");
	}

	@Test
	public void test12() {
		String s = "const int p = 10, * const *q = 0, **const q = 0;";
		checkTypes(s, "(INT CONST)", "(* (* CONST (INT CONST)))", "(* CONST (* (INT CONST)))");
	}

	@Test
	public void test13() {
		String s = "int apply(int (*f)(int, int), int, int);";
		checkTypes(s, "(() (INT) (* (() (INT) (INT) (INT))) (INT) (INT))");
	}

	@Test
	public void test14() {
		String s = "const MyType * const x;";
		checkTypes(s, "(* CONST (MyType CONST))");
	}

	@Test
	public void test15() {
		String s = "struct MyStruct s;";
		checkTypes(s, "(STRUCT MyStruct)");
	}

	@Test
	public void test16() {
		String s = "union MyUnion s;";
		checkTypes(s, "(UNION MyUnion)");
	}

	@Test
	public void test17() {
		String s = "struct MyStruct { int x; } s;";
		checkTypes(s, null, "(STRUCT MyStruct)");
	}

	@Test
	public void test18() {
		String s = "struct {int x; } s;";
		checkTypes(s, null, "(STRUCT?)");
		debugDecl(s);
	}

	@Test
	public void test19() {
		String s = "struct MyStruct { int tag; union { int x; float y; } u; } s;";
		checkTypes(s, null, "(STRUCT MyStruct)");
		debugDecl(s);
	}
}
