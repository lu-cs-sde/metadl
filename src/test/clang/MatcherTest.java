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
import lang.c.pat.ast.FunctionDefinition;
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

  public static <T extends ASTNode> List<String> genMatchers(String s, Category c) {
    List<String> matchers = new ArrayList<>();
    List<T> res = TypeTest.parse(s, c);

    for (T t : res) {
      List<MatcherBuilder> mbs = ClangEvaluationContext.genMatcherBuilder(t);
      for (MatcherBuilder mb : mbs) {
        String m = mb.generate();
        System.err.println(m);
        matchers.add(m);
      }
    }

    return matchers;
  }

  @Test public void test5() {
    List<String> res = genMatchers("int x;", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(qualType(isInteger())), unless(hasInitializer(anything())))",  res.get(0));
  }

  @Test public void test6() {
    List<String> res = genMatchers("int *x;", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(qualType(pointsTo(qualType(isInteger())))), unless(hasInitializer(anything())))", res.get(0));
  }

  @Test public void test7() {
    List<String> res = genMatchers("int x[];", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasName(\"x\"), hasType(arrayType(hasElementType(qualType(isInteger())))), unless(hasInitializer(anything())))", res.get(0));
  }

  @Test public void test8() {
    List<String> res = genMatchers("int *$v[10][20];", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasType(arrayType(hasElementType(arrayType(hasElementType(qualType(pointsTo(qualType(isInteger())))))))), unless(hasInitializer(anything()))).bind(\"$v\")", res.get(0));
  }

  @Test public void test9() {
    List<String> res = genMatchers("int (*$a[10])(int);", PatLangParserSEP.n_declaration);
    assertEquals(1, res.size());
    assertEquals("varDecl(hasType(arrayType(hasElementType(qualType(pointsTo(ignoringParens(functionProtoType(parameterCountIs(1), hasParameterType(0, qualType(isInteger())), hasReturnType(qualType(isInteger()))))))))), unless(hasInitializer(anything()))).bind(\"$a\")", res.get(0));
  }

  @Test public void test10() {
    List<String> res = genMatchers("while ($cond) $body", PatLangParserSEP.n_statement);
    assertEquals(1, res.size());
    assertEquals("whileStmt(hasCondition(expr().bind(\"$cond\")), hasBody(stmt().bind(\"$body\")))",
                 res.get(0));
  }

  @Test public void test11() {
    List<String> res = genMatchers("for ($init; ;) $body", PatLangParserSEP.n_statement);
    assertEquals(2, res.size());
    // This matches for loops such as for(float; ; ) { .. } where the declaration does not declare anything. This is allowed by C, but it
    // has no representation in clang AST. Such declarations are just discarded.
    assertEquals("forStmt(hasLoopInit(declStmt(declCountIs(0), declDistinct())), unless(hasCondition(anything())), unless(hasIncrement(anything())), hasBody(stmt().bind(\"$body\")))", res.get(1));
    assertEquals("forStmt(hasLoopInit(expr().bind(\"$init\")), unless(hasCondition(anything())), unless(hasIncrement(anything())), hasBody(stmt().bind(\"$body\")))", res.get(0));
  }

  @Test public void test12() {
    List<String> res = genMatchers("for ($init ;) $body", PatLangParserSEP.n_statement);
    assertEquals(1, res.size());
    assertEquals("forStmt(hasLoopInit(stmt().bind(\"$init\")), unless(hasCondition(anything())), unless(hasIncrement(anything())), hasBody(stmt().bind(\"$body\")))", res.get(0));
  }

  @Test public void test13() {
    List<String> res = genMatchers("struct S { int x; } s;", PatLangParserSEP.n_declaration);
    assertEquals(2, res.size());
    assertEquals("recordDecl(hasName(\"S\"), isStruct(), fieldDistinct(fieldDecl(hasName(\"x\"), hasType(qualType(isInteger())))))", res.get(0));
    assertEquals("varDecl(hasName(\"s\"), hasType(recordDecl(hasName(\"S\"), isStruct())), unless(hasInitializer(anything())))", res.get(1));
  }

  @Test public void test14() {
    List<String> res = genMatchers("struct $s { int x; } s;", PatLangParserSEP.n_declaration);
    assertEquals(2, res.size());
    assertEquals("recordDecl(isStruct(), fieldDistinct(fieldDecl(hasName(\"x\"), hasType(qualType(isInteger()))))).bind(\"$s\")",
                 res.get(0));
    assertEquals("varDecl(hasName(\"s\"), hasType(recordDecl(isStruct()).bind(\"$s\")), unless(hasInitializer(anything())))",
                 res.get(1));
  }

  @Test public void test15() {
    List<String> res = genMatchers("$f($a, ..)", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("callExpr(callee(expr().bind(\"$f\")), argumentDistinct(expr().bind(\"$a\")))",
        res.get(0));
  }

  @Test public void test16() {
    List<String> res = genMatchers("void f() {..}", PatLangParserSEP.n_function_definition);
    assertEquals(1, res.size());
    assertEquals(
        "functionDecl(parameterCountIs(0), hasReturnTypeLoc(loc(qualType(isVoid()))), hasType(ignoringParens(functionNoProtoType())), hasBody(compoundStmt(subStatementDistinct())))",
        res.get(0));
  }

  @Test public void test17() {
    List<String> res = genMatchers("void f(void) {..}", PatLangParserSEP.n_function_definition);
    assertEquals(1, res.size());
    assertEquals(
        "functionDecl(parameterCountIs(0), hasReturnTypeLoc(loc(qualType(isVoid()))), hasType(ignoringParens(functionProtoType())), hasBody(compoundStmt(subStatementDistinct())))",
        res.get(0));
  }

  @Test public void test18() {
    List<String> res = genMatchers("s.a", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("memberExpr(unless(isArrow()), has(declRefExpr(to(namedDecl(hasName(\"s\"))))), member(fieldDecl(hasName(\"a\"))))",
                 res.get(0));
  }

  @Test public void test19() {
    List<String> res = genMatchers("$s.$a", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("memberExpr(unless(isArrow()), has(expr().bind(\"$s\")), member(fieldDecl().bind(\"$a\")))",
                 res.get(0));
  }

  @Test public void test20() {
    List<String> res = genMatchers("s->a", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("memberExpr(isArrow(), has(declRefExpr(to(namedDecl(hasName(\"s\"))))), member(fieldDecl(hasName(\"a\"))))", res.get(0));
  }

  @Test public void test21() {
    List<String> res = genMatchers("$s->$a", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("memberExpr(isArrow(), has(expr().bind(\"$s\")), member(fieldDecl().bind(\"$a\")))",
                 res.get(0));
  }


  @Test public void test22() {
    List<String> res = genMatchers("$s.$f.$g", PatLangParserSEP.n_expression);
    assertEquals(1, res.size());
    assertEquals("memberExpr(unless(isArrow()), has(memberExpr(unless(isArrow()), has(expr().bind(\"$s\")), member(fieldDecl().bind(\"$f\")))), member(fieldDecl().bind(\"$g\")))", res.get(0));
  }





}
