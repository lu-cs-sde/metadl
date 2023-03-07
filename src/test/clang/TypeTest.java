package clang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.StringReader;

import org.junit.jupiter.api.Disabled;
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

	@Test
	public void test4() {
		String s = "const int * const p = 10, q = 11;";
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}

	@Test
	public void test5() {
		String s = "int p, q;";
		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}

	}

	@Test
	public void test6() {
		String s = "const int p[const 10];";

		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}

	@Test
	public void test7() {
		String s = "int foo(int);";

		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}

	@Test
	public void test8() {
		String s = "int foo(int n);";

		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}


	@Test
	@Disabled
	public void test9() {
		String s = "int (*foo)(int);";

		Declaration d = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
		d.debugPrint(System.out);

		System.out.println("========================================");
		for (AST.Decl clangDecl : d.clangDecls()) {
			clangDecl.prettyPrint(System.out);
		}
	}



}
