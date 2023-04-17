package lang;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.StringReader;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import beaver.Symbol;
import se.lth.sep.Category;
import se.lth.sep.ParseTree;


public class CPatternTest {
	private static void testAST(String c, Category start) {
		System.out.println("================================================================================");
		System.out.println("Parsing: \"" + c + "\"");
		System.out.println("================================================================================");

		java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(c)));

		var astBuilder = lang.c.pat.ast.ASTBuilder.getInstance();
		java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, start, astBuilder,
															   lang.c.pat.ast.PatLangParserSEP.Terminals.NAMES);

		assertNotNull(parseTrees);

		for (lang.c.pat.ast.ASTNode ast : CTestUtil.<lang.c.pat.ast.ASTNode>buildAST(tokens, parseTrees, astBuilder)) {
			ast.debugPrint(System.out);
		}
	}

	@Test
  @Disabled
	public void test1() {
		testAST("$t1 f(void) { .. for ($_; $_; $_) { .. } .. }", lang.c.pat.ast.PatLangParserSEP.n_function_definition);
	}

  @Test
  @Disabled
  public void test2() {
    testAST("for($init ; ) $body", lang.c.pat.ast.PatLangParserSEP.n_statement);
  }

  @Test
  public void test3() {
    testAST("struct S { int x; } s;", lang.c.pat.ast.PatLangParserSEP.n_declaration);
  }
}
