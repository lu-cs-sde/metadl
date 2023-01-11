package clang;

import java.io.StringReader;
import java.util.List;
import lang.c.pat.ast.PatLangParserSEP;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import beaver.Symbol;
import lang.CTestUtil;
import se.lth.sep.Category;
import se.lth.sep.ParseTree;


public class MatcherTest {
	private static List<lang.c.pat.ast.ASTNode> parse(String c, Category start) {
		// System.out.println("================================================================================");
		// System.out.println("Parsing: \"" + c + "\"");
		// System.out.println("================================================================================");

		java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(c)));

		java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, start);

		assertNotNull(parseTrees);

		return CTestUtil.buildAST(tokens, parseTrees);
	}

	@Test public void test1() {
		var roots = parse("for(;;)  ;", PatLangParserSEP.n_for_statement);

		for (var root : roots) {
			System.out.println(root.matcher().genMatcher());
		}
	}

	@Test public void test2() {
		var roots = parse("for(;;) $body ", PatLangParserSEP.n_for_statement);

		for (var root : roots) {
			System.out.println(root.matcher().genMatcher());
		}
	}

	@Test public void test3() {
		var roots = parse("for($init; $cond; $inc) $body ", PatLangParserSEP.n_for_statement);

		for (var root : roots) {
			System.out.println(root.matcher().genMatcher());
		}
	}

	@Test public void test4() {
		var roots = parse("for($init $cond; $inc) $body ", PatLangParserSEP.n_for_decl_statement);

		for (var root : roots) {
			System.out.println(root.matcher().genMatcher());
		}
	}
}
