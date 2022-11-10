package lang;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DisplayName;
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
			"naive_call_graph"
		};
		return Arrays.stream(tests);
	}

	@DisplayName("Evaluate MetaDL-C programs with the internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlCTests")
	void evaluationTestMetaDLCInternal(String fileName) throws Exception {
		String analyzedFilesDir = "tests/clang/evaluation/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.c");

		Util.singleEvaluationTest("tests/output/",
							 "tests/clang/evaluation/facts",
							 analyzedFiles,
							 "tests/clang/evaluation",
							 "tests/clang/evaluation/expected",
							 fileName,
							 ".mdl",
							 "c",
							 "-e metadl");
	}

	@DisplayName("Evaluate MetaDL-C programs with the parallel internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlCTests")
	void evaluationTestMetaDLCParallelInternal(String fileName) throws Exception {
		String analyzedFilesDir = "tests/clang/evaluation/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.c");

		Util.singleEvaluationTest("tests/output/",
							 "tests/clang/evaluation/facts",
							 analyzedFiles,
							 "tests/clang/evaluation",
							 "tests/clang/evaluation/expected",
							 fileName,
							 ".mdl",
							 "c",
							 "-e metadl-par");
	}

}
