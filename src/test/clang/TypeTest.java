package clang;

import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.StringReader;
import java.util.List;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import beaver.Symbol;
import lang.CTestUtil;
import lang.c.pat.ast.ASTNode;
import lang.c.pat.ast.Declaration;
import se.lth.sep.Category;
import se.lth.sep.ParseTree;

public class TypeTest {
  public static <T extends ASTNode> List<T> parse(String s, Category startSymbol) {
    java.util.List<Symbol> tokens = CTestUtil.scan(new lang.c.pat.ast.PatLangScanner(new StringReader(s)));

    var astBuilder = lang.c.pat.ast.ASTBuilder.getInstance();
    java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, startSymbol, astBuilder,
                                                           lang.c.pat.ast.PatLangParserSEP.Terminals.NAMES);

    assertNotNull(parseTrees);

    if (parseTrees.size() > 1) {
      se.lth.sep.Util.dumpParseTrees("ambigous_parse_", parseTrees);
    }

    // assertEquals(1, parseTrees.size(), "Expecting an unambigous parse");

    return CTestUtil.<T>buildAST(tokens, parseTrees, astBuilder);
  }

  @Test
  public void test1() {
    String s = "int *array[10];";
    debugDecl(s);
  }

  @Test
  public void test2() {
    String s = "int (*array)[10];";
    debugDecl(s);
  }

  @Test
  public void test3() {
    String s = "const int * const p = 10;";
    debugDecl(s);
  }

  private static void debugDecl(String s) {
    List<Declaration> ds = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);
    //d.debugPrint(System.out);

    for (Declaration d : ds) {
      System.out.println("========================================");
      d.debugPrint(System.out);
      for (AST.Decl clangDecl : d.clangDecls()) {
        clangDecl.prettyPrint(System.out);
      }
    }
  }

  private static void checkDecl(String s, String ... checks) {
    List<Declaration> ds = parse(s, lang.c.pat.ast.PatLangParserSEP.n_declaration);

    for (Declaration d : ds) {
      List<AST.Decl> clangDecls = d.clangDecls();
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      for (AST.Decl clangDecl : clangDecls) {
        clangDecl.prettyPrint(new PrintStream(bos));
      }
      String[] lines = bos.toString().split("\\R");
      int j = 0;
      for (int i = 0; i < checks.length; ++i) {
        for (; j < lines.length; ++j) {
          if (Pattern.matches(".*" + checks[i] + ".*", lines[j]))
            break;
        }
        if (j == lines.length) {
          fail("Check pattern '" + checks[i] + "' failed to match.");
        }
      }
    }
  }



  private static void checkTypes(String decl, String ... expectedTypes) {
    List<Declaration> ds = parse(decl, lang.c.pat.ast.PatLangParserSEP.n_declaration);

    for (Declaration d : ds) {
      List<AST.Decl> result = d.clangDecls();
      for (int i = 0; i < result.size(); ++i) {
        if (expectedTypes[i] == null)
          continue;
        assertEquals(expectedTypes[i], result.get(i).explicitType.printAsType());
      }
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

  @Test
  public void test20NoMetaVar() {
    String s = "int *p;";
    debugDecl(s);
  }

  // An now metavariables
  @Test
  public void test20() {
    String s = "int *$p;";
    debugDecl(s);
  }

  @Test
  public void test21qual() {
    String s = "const int *$p;";
    debugDecl(s);
  }

  @Test
  public void test22() {
    String s = "int $p, *$q;";
    debugDecl(s);
  }

  @Test
  public void test23() {
    String s = "int $p(void), x = 1;";
    debugDecl(s);
  }

  @Test
  public void test123() {
    String s = "int[] p(void);";
    debugDecl(s);
  }


  @Test
  public void test24() {
    String s = "int $f(int $q);";
    debugDecl(s);
  }

  @Test
  public void test25() {
    String s = "int (*$f)(int $q);";
    debugDecl(s);
    checkDecl(s, "\\$f.*:=.*VarDecl");
  }

  @Test
  public void test26() {
    String s = "int f(int, .., int);";
    debugDecl(s);
    checkDecl(s, "FunctionDecl",
              "ParmVarDecl.*INT",
              "\\.\\.\\..*ParmVarDecl",
              "ParmVarDecl.*INT");
  }

  @Test
  public void test27() {
    String s = "int x, .., y;";
    debugDecl(s);
  }

  @Test
  public void test28() {
    String s = "int .., $x, ..;";
    debugDecl(s);
  }
}
