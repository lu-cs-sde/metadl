package lang;
import java.util.ArrayList;
import java.util.stream.Collectors;

import beaver.Symbol;
import beaver.Scanner;
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


  public static java.util.List<ParseTree> parse(java.util.List<Symbol> tokens,
                                                Category startSymbol,
                                                lang.ast.ASTBuilder astBuilder,
                                                String[] tokenNames) {
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
                                                                        if (p.getName().equals("declarator"))
                                                                          return false;
                                                                        if (p.getName().equals("typedef_name"))
                                                                          return false;
                                                                        if (c.getName().equals("METAVARID"))
                                                                          return true;
                                                                        if (c.getName().equals("GAP"))
                                                                          return true;
                                                                        return false;
                                                                      }
                                                                    });
    for (int i = 0; i < parseTrees.size(); ++i) {
      Util.dumpParseTree("dump_" + i + ".dot", parseTrees.get(i));
    }
    return parseTrees;
  }


  public static <T> java.util.List<T> buildAST(java.util.List<Symbol> tokens,
                                               java.util.List<ParseTree> parseTrees,
                                               lang.ast.ASTBuilder builder) {
    var list = new ArrayList<T>();

    var astBuilder = builder;

    for (ParseTree pt : parseTrees) {
      T root = (T) astBuilder.buildAST(pt, tokens);
      list.add(root);
    }
    return list;
  }
}
