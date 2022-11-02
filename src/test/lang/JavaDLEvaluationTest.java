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

public class JavaDLEvaluationTest {
	static Stream<String> metadlJavaTests() {
		String[] tests = {
			"bad-covariant-equals", "switch-no-default", "clone-idioms",
			//"number-ctor",
			// TODO: re-enable number-ctor once conversion functors node -> int and int -> node
			// are introduced
			"unwritten-field", "naming-convention", "reference-to-mutable-object", "missing-override",
			"reference-equality", "boxed-primitive-constructor", "operator-precedence",
			"type-param-unused-in-formals", "paper-examples", "java7", "java8",
			// "attr-provenance": provenance information is not produced in non-incremental mode
			"class-hierarchy-utils",
			"names"
		};
		return Arrays.stream(tests);
	}


	@DisplayName("Evaluate MetaDL-Java programs with Souffle")
	@ParameterizedTest
	@MethodSource("metadlJavaTests")
	void evaluationTestMetaDLJavaSouffle(String fileName) throws Exception {
		String analyzedFilesDir = "tests/evaluation/metadl-java/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.java");

		Util.singleEvaluationTest("tests/output/souffle",
							 "tests/evaluation/metadl-java/facts",
							 analyzedFiles,
							 "tests/evaluation/metadl-java",
							 "tests/evaluation/metadl-java/expected",
							 fileName,
							 ".mdl",
							 "java",
							 "-e souffle");
	}

	@DisplayName("Evaluate MetaDL-Java programs with the internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlJavaTests")
	void evaluationTestMetaDLJavaInternal(String fileName) throws Exception {
		String analyzedFilesDir = "tests/evaluation/metadl-java/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.java");

		Util.singleEvaluationTest("tests/output/",
							 "tests/evaluation/metadl-java/facts",
							 analyzedFiles,
							 "tests/evaluation/metadl-java",
							 "tests/evaluation/metadl-java/expected",
							 fileName,
							 ".mdl",
							 "java",
							 "-e metadl");
	}
}
