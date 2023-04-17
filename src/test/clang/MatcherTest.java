package clang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import beaver.Symbol;
import lang.CTestUtil;
import lang.c.pat.ast.ASTNode;
import lang.c.pat.ast.Declaration;
import lang.c.pat.ast.Expression;
import lang.c.pat.ast.PatLangParserSEP;
import lang.c.pat.ast.Statement;
import se.lth.sep.Category;
import se.lth.sep.ParseTree;


public class MatcherTest {
  private static List<lang.c.pat.ast.ASTNode> parse(String c, Category start) {
    // System.out.println("================================================================================");
    // System.out.println("Parsing: \"" + c + "\"");
    // System.out.println("================================================================================");

    java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(c)));

    var astBuilder = lang.c.pat.ast.ASTBuilder.getInstance();
    java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, start,
                                                           astBuilder,
                                                           lang.c.pat.ast.PatLangParserSEP.Terminals.NAMES);

    assertNotNull(parseTrees);

    return CTestUtil.buildAST(tokens, parseTrees, astBuilder);
  }

  @Test public void test1() {
    var roots = parse("for(;;)  ;", PatLangParserSEP.n_for_statement);

    for (var root : roots) {
      System.out.println(root.matcher().genMatcher());
    }
  }

  @Disabled
  @Test public void test2() {
    var roots = parse("for(;;) $body ", PatLangParserSEP.n_for_statement);

    for (var root : roots) {
      System.out.println(root.matcher().genMatcher());
    }
  }

  @Disabled
  @Test public void test3() {
    var roots = parse("for($init; $cond; $inc) $body ", PatLangParserSEP.n_for_statement);

    for (var root : roots) {
      System.out.println(root.matcher().genMatcher());
    }
  }

  @Disabled
  @Test public void test4() {
    var roots = parse("for($init $cond; $inc) $body ", PatLangParserSEP.n_for_decl_statement);

    for (var root : roots) {
      System.out.println(root.matcher().genMatcher());
    }
  }

  public static <T extends ASTNode> List<String> genMatchers(String s, Category c) {
    List<String> matchers = new ArrayList<>();
    List<T> res = TypeTest.parse(s, c);
    for (T internalNode : res) {
      Iterable<? extends AST.Node> clangNodes;
      if (internalNode instanceof Declaration) {
        clangNodes = ((Declaration) internalNode).clangDecls();
      } else if (internalNode instanceof Statement) {
        clangNodes = List.of(((Statement) internalNode).asClangStmt());
      } else {
        assertTrue(internalNode instanceof Expression);
        clangNodes = List.of(((Expression) internalNode).asClangExpr());
      }

      for (AST.Node clangDecl : clangNodes) {
        ASTMatcherGen gen = new ASTMatcherGen();
        clangDecl.acceptPO(gen);
        String pat = gen.lookup(clangDecl).generate();
        matchers.add(pat);
        System.err.println(pat);
      }
    }

    return matchers;
  }

  public static <T extends ASTNode> List<MatcherBuilder> genMatcherBuilders(String s, Category c) {
    List<MatcherBuilder> matchers = new ArrayList<>();
    List<T> res = TypeTest.parse(s, c);
    for (T internalNode : res) {
      Iterable<? extends AST.Node> clangNodes;
      if (internalNode instanceof Declaration) {
        clangNodes = ((Declaration) internalNode).clangDecls();
      } else if (internalNode instanceof Statement) {
        clangNodes = List.of(((Statement) internalNode).asClangStmt());
      } else {
        assertTrue(internalNode instanceof Expression);
        clangNodes = List.of(((Expression) internalNode).asClangExpr());
      }

      for (AST.Node clangDecl : clangNodes) {
        ASTMatcherGen gen = new ASTMatcherGen();
        clangDecl.acceptPO(gen);
        matchers.add(gen.lookup(clangDecl));
      }
    }
    return matchers;
  }

  @Test public void test5() {
    List<String> res = genMatchers("int x;", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(qualType(isInteger())))",  res.get(0));
  }

  @Test public void test6() {
    List<String> res = genMatchers("int *x;", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(qualType(pointsTo(qualType(isInteger())))))", res.get(0));
  }

  @Test public void test7() {
    List<String> res = genMatchers("int x[];", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(arrayType(hasElementType(qualType(isInteger())))))", res.get(0));
  }

  @Test public void test8() {
    List<String> res = genMatchers("int *$v[10][20];", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasType(arrayType(hasElementType(arrayType(hasElementType(qualType(pointsTo(qualType(isInteger()))))))))).bind(\"$v\")", res.get(0));
  }

  @Test public void test9() {
    List<String> res = genMatchers("int (*$a[10])(int);", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasType(arrayType(hasElementType(qualType(pointsTo(ignoringParens(functionProtoType(parameterCountIs(1), hasParameterType(0, qualType(isInteger())), hasReturnType(qualType(isInteger())))))))))).bind(\"$a\")", res.get(0));
  }

  @Test public void test10() {
    List<String> res = genMatchers("while ($cond) $body", PatLangParserSEP.n_statement);
    assertEquals(1, res.size());
    assertEquals("whileStmt(hasCondition(expr().bind(\"$cond\")), hasBody(stmt().bind(\"$body\")))",
                 res.get(0));
  }

  @Test public void test11() {
    List<String> res = genMatchers("for ($init; ;) $body", PatLangParserSEP.n_statement);
    assertEquals(1, res.size());
    assertEquals("forStmt(hasLoopInit(expr().bind(\"$init\")), unless(hasCondition(anything())), unless(hasIncrement(anything())), hasBody(stmt().bind(\"$body\")))", res.get(0));
  }

  @Test public void test12() {
    List<String> res = genMatchers("for ($init ;) $body", PatLangParserSEP.n_statement);
    assertEquals(1, res.size());
    assertEquals("forStmt(hasLoopInit(stmt().bind(\"$init\")), unless(hasCondition(anything())), unless(hasIncrement(anything())), hasBody(stmt().bind(\"$body\")))", res.get(0));
  }

  @Test public void test13() {
    List<String> res = genMatchers("struct S { int x; } s;", PatLangParserSEP.n_declaration);
    // System.out.println(res.get(0));
    assertEquals(2, res.size());
    assertEquals("recordDecl(hasName(\"S\"), isStruct(), fieldDistinct(fieldDecl(hasName(\"x\"), hasType(qualType(isInteger())))))", res.get(0));
    assertEquals("varDecl(hasName(\"s\"), hasType(recordDecl(hasName(\"S\"), isStruct())))", res.get(1));
  }
}
