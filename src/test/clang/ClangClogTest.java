package clang;

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import clang.swig.ClangClog;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorLong;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;
import lang.c.pat.ast.PatLangParserSEP;

public class ClangClogTest {
  private static String[] inputCFiles = {
    "tests/clang/evaluation/src/loop_depth/call_in_loop.c",
    "tests/clang/evaluation/src/loop_depth/duff.c",
    "tests/clang/evaluation/src/loop_depth/loops.c"
  };

  private static ClangClog clog;

  @BeforeAll
  public static void prepare() {
    try {
      System.loadLibrary("clangClogSWIG");
    } catch (SecurityException | UnsatisfiedLinkError e) {
      Assumptions.assumeTrue(false);
    }

    List<String> tests = List.of(inputCFiles);

    ClangClogBuilder builder = new ClangClogBuilder(new VectorString(tests));
    clog = builder.build();
    if (!clog.init()) {
      fail("Error initializing clang-clog");
    }
  }

  @Test
  public void test1() {
    long declMatcherHandle = clog.registerMatcher("decl().bind(\"d\")", true);
    assertTrue(declMatcherHandle >= 0);
    long forStmtHandle = clog.registerMatcher("forStmt().bind(\"f\")", false);
    assertTrue(forStmtHandle >= 0);
    clog.runGlobalMatchers();

    VectorVectorLong declResult = clog.matchFromRoot((int)declMatcherHandle);

    for (VectorLong row : declResult) {
      for (long e : row) {
        System.out.print(e + " ");
      }
      System.out.println();
    }
  }

  @Test
  public void test2() {
    List<MatcherBuilder> matchers = MatcherTest.genMatcherBuilders("int (*$f)(int $q);", PatLangParserSEP.n_declaration);
    assertTrue(!matchers.isEmpty());
    MatcherBuilder first = matchers.iterator().next();
    SortedSet<String> metavars = first.bindings();
    Map<MatcherBuilder, Long> matchHandles = new HashMap<>();
    for (MatcherBuilder b : matchers) {
      System.out.println("Registering '" + b.generate() + "'");
      long h = clog.registerMatcher(b.generate(), true);
      matchHandles.put(b, h);
      assertEquals(metavars, b.bindings());
    }

    clog.runGlobalMatchers();

    for (MatcherBuilder b : matchers) {
      for (String mv : metavars) {
        System.out.print(mv + "\t");
      }
      System.out.println();

      long h = matchHandles.get(b);

      VectorVectorLong result = clog.matchFromRoot((int)h);
      for (VectorLong row : result) {
        for (long e : row) {
          System.out.println(e + "\t");
        }
        System.out.println();
      }
      System.out.println("================================================================================");
    }
  }
}
