package lang;

import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import lang.io.FileUtil;

public class CEvaluationTest {
  static Stream<String> metadlCTests() {
    String[] tests = {
      "first_test",
      "nested_for_loops",
      "nested_loops",
      "function",
      "naive_call_graph",
      "loop_depth",
      "macro_expansion"
    };
    return Arrays.stream(tests);
  }

  private static void runTest(String fileName, boolean parallelEval) throws Exception {
    String analyzedFilesDir = "tests/clang/evaluation/src";
    List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.c", "*.cpp");
    Path includeDir = Path.of(analyzedFilesDir, "include");

    Util.singleEvaluationTest("tests/output/",
                              "tests/clang/evaluation/facts",
                              analyzedFiles,
                              "tests/clang/evaluation",
                              "tests/clang/evaluation/expected",
                              fileName,
                              ".mdl",
                              "c4",
                              parallelEval ? "-e metadl-par" : "-e metadl",
                              "--extra-arg -I" + includeDir.toAbsolutePath());
  }

  private static void runTest(String fileName) {
    try {
      runTest(fileName, false);
    } catch (Exception e) {
      System.err.println("Unexpected exception " + e);
      fail();
    }
  }

  @Test void first_test() {
    runTest("first_test");
  }

  @Test void nested_for_loops() {
    runTest("nested_for_loops");
  }

  @Test void nested_loops() {
    runTest("nested_loops");
  }

  @Test void function() {
    runTest("function");
  }

  @Test void naive_call_graph() {
    runTest("naive_call_graph");
  }

  @Test void loop_depth() {
    runTest("loop_depth");
  }

  @Test void macro_expansion() {
    runTest("macro_expansion");
  }

  @Test void uninitialized_variable() {
    runTest("uninitialized_variable");
  }

  @Test void uninit_var_and_mem() {
    runTest("uninit_var_and_mem");
  }

  @Test void use_after_free() {
    runTest("use_after_free");
  }

  @Test void cg() {
    runTest("cg");
  }

  @Test void os_command_injection() {
    runTest("os_command_injection");
  }
}
