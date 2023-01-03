package lang;
import java.util.ArrayList;
import java.util.stream.Collectors;

import beaver.Symbol;
import beaver.Scanner;
import lang.c.pat.ast.ASTBuilder;
import se.lth.sep.Category;
import se.lth.sep.EarleyParser;
import se.lth.sep.Grammar;
import se.lth.sep.ParseTree;
import se.lth.sep.SPPFNode;
import se.lth.sep.SPPFTrivialProductionRemover;
import se.lth.sep.Util;


public class CTestUtil {
	public static java.util.List<Symbol> scan(Scanner scanner) {
		ArrayList<Symbol> result = new ArrayList<>();
		do {
			beaver.Symbol sym;
			try {
				sym = scanner.nextToken();
			} catch (Exception e) {
				System.out.println(e);
				break;
			}

			if (sym == null || sym.getId() == 0 /*EOF*/) {
				break;
			} else if (sym.getId() < 0) {
				// layout symbols, ignore
			} else {
				result.add(sym);
			}
		} while (true);

		return result;
	}


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



}
