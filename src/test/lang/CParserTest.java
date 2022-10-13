package lang;

import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.StringReader;
import org.apache.commons.lang3.tuple.Pair;
import lang.c.obj.ast.ObjLangParserSEP;
import lang.c.obj.ast.ASTBuilder;
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
		var result = new ArrayList<Symbol>();

		Scanner scanner = new lang.c.obj.ast.ObjLangScanner(new StringReader(s));
		String[] tokenNames = lang.c.obj.ast.ObjLangParserSEP.Terminals.NAMES;

		do {
			beaver.Symbol sym;
			try {
				sym = scanner.nextToken();
				System.out.println(sym);
			} catch (Exception e) {
				System.out.println(e);
				return null;
			}
			if (sym.getId() != 0 /*EOF*/) {
				result.add(sym);
			} else {
				break;
			}
		} while (true);

		return result;
	}

	public static java.util.List<ParseTree> parse(java.util.List<Symbol> tokens, Category startSymbol) {
		String[] tokenNames = lang.c.obj.ast.ObjLangParserSEP.Terminals.NAMES;
		ASTBuilder astBuilder = ASTBuilder.getInstance();
		Grammar grammar = astBuilder.getGrammar();

		Category[] tokenCats = tokens.stream().map(sym -> grammar.getCategory(tokenNames[sym.getId()]))
			.collect(Collectors.toList()).toArray(new Category[1]);


		EarleyParser parser = astBuilder.getParser();

		SPPFNode root = parser.parse(tokenCats, startSymbol);
		if (root == null)
			return null;

		java.util.List<ParseTree> parseTrees = Util.enumerateParseTrees(root, astBuilder.getGrammar(),
																		new SPPFTrivialProductionRemover(astBuilder.getGrammar()));


		return parseTrees;
	}

	@Test
	public void test1() {
		String c = "int x;";
		Category start = lang.c.obj.ast.ObjLangParserSEP.n_declaration;

		java.util.List<ParseTree> parseTrees =  parse(scan(c), start);
		assertNotNull(parseTrees);

		Util.dumpParseTrees("test1_", parseTrees);
	}
}
