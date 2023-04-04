package clang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

import clang.swig.ClangClog;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorLong;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;
import lang.c.pat.ast.PatLangParserSEP;
import se.lth.sep.Category;

public class ClangClogTest {
  private static String[] inputCFiles = {
    "tests/clang/evaluation/src/loop_depth/call_in_loop.c",
    "tests/clang/evaluation/src/loop_depth/duff.c",
    "tests/clang/evaluation/src/loop_depth/loops.c"
  };

  public static ClangClog newClog(String ... srcs) {
    try {
      System.loadLibrary("clangClogSWIG");
    } catch (SecurityException | UnsatisfiedLinkError e) {
      Assumptions.assumeTrue(false);
    }

    List<String> tests = List.of(srcs);

    ClangClogBuilder builder = new ClangClogBuilder(new VectorString(tests));
    ClangClog clog = builder.build();
    if (!clog.init()) {
      fail("Error initializing clang-clog");
    }
    return clog;
  }

  private static List<Map<String, ClangClog.Loc>> matchPatternOnFile(String pat, Category c, String srcFile) {
    ClangClog clog = newClog(srcFile);
    List<MatcherBuilder> matchers = MatcherTest.genMatcherBuilders(pat, c);
    assertTrue(!matchers.isEmpty());
    MatcherBuilder first = matchers.iterator().next();
    SortedSet<String> metavars = first.bindings();
    Map<MatcherBuilder, Long> matchHandles = new HashMap<>();
    for (MatcherBuilder b : matchers) {
      if (b.bindings().isEmpty()) {
        b.bind("$__root");
        metavars.add("$__root");
      }
      System.out.println("Registering '" + b.generate() + "'");
      long h = clog.registerMatcher(b.generate(), true);
      matchHandles.put(b, h);
      assertEquals(metavars, b.bindings());
    }

    clog.runGlobalMatchers();

    List<Map<String, ClangClog.Loc>> matchResults = new ArrayList<>();

    for (MatcherBuilder b : matchers) {
      long h = matchHandles.get(b);
      VectorVectorLong result = clog.matchFromRoot(h);

      for (VectorLong row : result) {
        Iterator<Long> rit = row.iterator();
        Iterator<String> mvit = metavars.iterator();

        Map<String, ClangClog.Loc> matchResult = new HashMap<>();

        while (rit.hasNext() && mvit.hasNext()) {
          long nodeId = rit.next();
          ClangClog.Loc srcLoc = clog.srcLocation(nodeId);

          matchResult.put(mvit.next(), srcLoc);
        }

        assertFalse(rit.hasNext());
        assertFalse(mvit.hasNext());

        matchResults.add(matchResult);
      }
    }

    return matchResults;
  }

  @Test
  public void test1() {
    ClangClog clog = newClog(inputCFiles);
    long declMatcherHandle = clog.registerMatcher("decl().bind(\"d\")", true);
    assertTrue(declMatcherHandle >= 0);
    long forStmtHandle = clog.registerMatcher("forStmt().bind(\"f\")", false);
    assertTrue(forStmtHandle >= 0);
    clog.runGlobalMatchers();

    VectorVectorLong declResult = clog.matchFromRoot((int)declMatcherHandle);

    // for (VectorLong row : declResult) {
    //   for (long e : row) {
    //      System.out.print(e + " ");
    //   }
    //   System.out.println();
    // }
  }

  static class Checker {
    private Iterator<Map<String, ClangClog.Loc>> iterator;
    private Map<String, ClangClog.Loc> currentMap;

    public static Checker begin(Collection<Map<String, ClangClog.Loc>> result) {
      Checker ret = new Checker();
      ret.iterator = result.iterator();
      ret.currentMap = ret.iterator.next();
      return ret;
    }

    public Checker check(String metavar,
                      long startLine,
                      long endLine) {

      assertTrue(currentMap.containsKey(metavar));
      ClangClog.Loc loc = currentMap.get(metavar);
      assertEquals(startLine, loc.getStartLine());
      assertEquals(endLine, loc.getEndLine());
      return this;
    }

    public Checker check(String metavar,
                      long startLine,
                      long startCol,
                      long endLine,
                      long endCol) {
      check(metavar, startLine, endLine);
      assertEquals(startCol, currentMap.get(metavar).getStartCol());
      assertEquals(endCol, currentMap.get(metavar).getEndCol());
      return this;
    }

    public Checker next() {
      assertTrue(iterator.hasNext());
      currentMap = iterator.next();
      return this;
    }

    public void end() {
      assertFalse(iterator.hasNext());
    }
  }

  public static void dumpResults(Collection<Map<String, ClangClog.Loc>> results) {
    for (Map<String, ClangClog.Loc> r : results) {
      for (var entry : r.entrySet()) {
        String mv = entry.getKey();
        ClangClog.Loc loc = entry.getValue();
        System.out.println(mv + ": " + loc.getFilename() + ", " + loc.getStartLine() + ":" + loc.getStartCol() +
                           " - " + loc.getEndLine() + ":" + loc.getEndCol());
      }
    }
  }

  @Test
  public void test2() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("int (*$f)(int);", PatLangParserSEP.n_declaration, "tests/clang/clog/src/funcptr.c");
    dumpResults(results);
    Checker c = Checker.begin(results);
    c.check("$f", 1, 1).next();
    c.check("$f", 5, 10, 5, 22).next();
    c.check("$f", 12, 12).end();
  }

  @Test
  public void test3() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("int (*(*$f)(int))(int);", PatLangParserSEP.n_declaration, "tests/clang/clog/src/funcptr.c");
    dumpResults(results);
    Checker c = Checker.begin(results);
    c.check("$f", 7, 7).next();
    c.check("$f", 13, 13).end();
  }

  @Test
  public void test4() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("int (*$a[10])(int);", PatLangParserSEP.n_declaration, "tests/clang/clog/src/funcptr.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$a", 16, 16)
      .next()
      .check("$a", 17, 17)
      .end();
  }

  @Test
  public void test5() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("while($cond) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 2, 6)
      .check("$cond", 2, 10, 2, 10)
      .next()
      .check("$body", 4, 4)
      .check("$cond", 3, 12, 3, 12)
      .end();
  }

  @Test
  public void test6() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("do $body while($cond);", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 8, 10)
      .check("$cond", 10, 12, 10, 12)
      .next()
      .check("$body", 12, 6, 12, 6)
      .check("$cond", 12, 15, 12, 15)
      .end();
  }

  @Test
  public void test7() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("for ($init; $cond; $inc) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 17, 17)
      .check("$init", 17, 17)
      .check("$cond", 17, 17)
      .check("$inc", 17, 17)
      .end();
  }

  @Test
  public void test8() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("for (; ; ) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 20, 22)
      .end();
  }

  @Test
  public void test9() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("for ($init; ; ) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$init", 28, 28)
      .check("$body", 28, 30)
      .end();
  }

  @Test
  public void test10() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("for (; $cond; ) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 32, 34)
      .check("$cond", 32, 32)
      .end();
  }


  @Test
  public void test11() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("for (; ; $inc) $body", PatLangParserSEP.n_statement, "tests/clang/clog/src/loops.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$body", 36, 38)
      .check("$inc", 36, 36)
      .end();
  }

  @Test
  public void test12() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("if ($cond) $then", PatLangParserSEP.n_statement, "tests/clang/clog/src/if.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$then", 2, 9)
      .check("$cond", 2, 2)
      .end();
  }

  @Test
  public void test13() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("if ($cond) $then else $else", PatLangParserSEP.n_statement, "tests/clang/clog/src/if.c");
    dumpResults(results);

    Checker.begin(results)
      .check("$then", 3, 5)
      .check("$else", 5, 7)
      .check("$cond", 3, 3)
      .end();
  }

  @Test
  public void testCompoundStmt() {
    // match compound statements with single substatement
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("{ $stmt }", PatLangParserSEP.n_statement, "tests/clang/clog/src/block.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$stmt", 7, 7)
      .end();

  }

  @Test
  public void testCompoundStmt2() {
    // match compound statements with precisely 2 substatements
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("{ $stmt1 $stmt2 }", PatLangParserSEP.n_statement, "tests/clang/clog/src/block.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$stmt1", 11, 11)
      .check("$stmt2", 12, 12)
      .end();

  }

  @Test
  public void testCompoundStmtGap() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("{ .. }", PatLangParserSEP.n_statement, "tests/clang/clog/src/block.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$__root", 1, 20)
      .next()
      .check("$__root", 3, 3)
      .next()
      .check("$__root", 4, 4)
      .next()
      .check("$__root", 6, 8)
      .next()
      .check("$__root", 10, 13)
      .next()
      .check("$__root", 15, 19)
      .end();
  }

  @Test
  public void testCompoundStmtEmpty() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("{ }", PatLangParserSEP.n_statement, "tests/clang/clog/src/block.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$__root", 3, 3)
      .next()
      .check("$__root", 4, 4)
      .end();
  }

  @Test
  public void testCompoundStmtGap2() {
    List<Map<String, ClangClog.Loc>> results = matchPatternOnFile("{ $stmt1 ..  $stmt2 .. $stmt3 .. $stmt4 }", PatLangParserSEP.n_statement, "tests/clang/clog/src/block.c");
    dumpResults(results);
    Checker.begin(results)
      .check("$stmt1", 3, 3)
      .check("$stmt2", 4, 4)
      .check("$stmt3", 6, 8)
      .check("$stmt4", 10, 13)
      .next()
      .check("$stmt1", 3, 3)
      .check("$stmt2", 4, 4)
      .check("$stmt3", 6, 8)
      .check("$stmt4", 15, 19)
      .next()
      .check("$stmt1", 3, 3)
      .check("$stmt2", 4, 4)
      .check("$stmt3", 10, 13)
      .check("$stmt4", 15, 19)
      .next()
      .check("$stmt1", 3, 3)
      .check("$stmt2", 6, 8)
      .check("$stmt3", 10, 13)
      .check("$stmt4", 15, 19)
      .next()
      .check("$stmt1", 4, 4)
      .check("$stmt2", 6, 8)
      .check("$stmt3", 10, 13)
      .check("$stmt4", 15, 19)
      .end();
  }
}
