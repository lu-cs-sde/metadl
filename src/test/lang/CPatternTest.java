package lang;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.StringReader;
import org.apache.commons.lang3.tuple.Pair;
import lang.c.pat.ast.PatLangParserSEP;
import lang.c.pat.ast.ASTBuilder;
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


public class CPatternTest {

	public static java.util.List<ParseTree> parse(java.util.List<Symbol> tokens, Category startSymbol) {
		String[] tokenNames = lang.c.pat.ast.PatLangParserSEP.Terminals.NAMES;
		ASTBuilder astBuilder = ASTBuilder.getInstance();
		Grammar grammar = astBuilder.getGrammar();

		Category[] tokenCats = tokens.stream().map(sym -> grammar.getCategory(tokenNames[sym.getId()]))
			.collect(Collectors.toList()).toArray(new Category[0]);


		EarleyParser parser = astBuilder.getParser();

		SPPFNode root = parser.parse(tokenCats, startSymbol);
		if (root == null)
			return null;

		// Util.dumpParseResult("parse.dot", root, grammar);

		java.util.List<ParseTree> parseTrees = Util.enumerateParseTrees(root, astBuilder.getGrammar(),
																		new SPPFTrivialProductionRemover(astBuilder.getGrammar()) {
																			@Override public boolean isBubleUpChild(Category p, Category c) {
																				if (c.getName().equals("METAVARID"))
																					return true;
																				if (c.getName().equals("GAP"))
																					return true;
																				return false;
																			}
																		});
		return parseTrees;
	}

	public static java.util.List<lang.c.pat.ast.ASTNode> buildAST(java.util.List<Symbol> tokens,
															  java.util.List<ParseTree> parseTrees) {
		var list = new ArrayList<lang.c.pat.ast.ASTNode>();

		var astBuilder = ASTBuilder.getInstance();

		for (ParseTree pt : parseTrees) {
			lang.c.pat.ast.ASTNode root = (lang.c.pat.ast.ASTNode) astBuilder.buildAST(pt, tokens);
			list.add(root);
		}
		return list;
	}

	private static void testAST(String c, Category start) {
		System.out.println("================================================================================");
		System.out.println("Parsing: \"" + c + "\"");
		System.out.println("================================================================================");

		java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(c)));

		java.util.List<ParseTree> parseTrees = parse(tokens, start);

		assertNotNull(parseTrees);

		for (lang.c.pat.ast.ASTNode ast : buildAST(tokens, parseTrees)) {
			ast.debugPrint(System.out);
		}
	}

	@Test
	public void test1() {
		testAST("$t1 f(void) { .. for ($_; $_; $_) { .. } .. }", lang.c.pat.ast.PatLangParserSEP.n_function_definition);
	}
}
