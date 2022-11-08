package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Iterator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.io.filefilter.FileFileFilter;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.apache.commons.io.filefilter.TrueFileFilter;

import org.apache.commons.io.FileUtils;
import org.junit.Ignore;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import eval.EvaluationContext;
import eval.Relation2;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.PredicateSymbol;
import lang.ast.PredicateType;
import lang.ast.Literal;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.PseudoTuple;
import lang.relation.RelationWrapper;

public class EvaluationTest {
	@DisplayName("Compare Internal Evaluation to Souffle")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in", "evalTest_3.in", "evalTest_4.in", "evalTest_5.in",
							 "evalTest_6.in", "evalTest_7.in", "evalTest_8.in", "evalTest_9.in", "evalTest_13.in",
							 "evalTest_14.in"})
	void evaluationTestsBottomUpNaiveCompareSouffle(String fileName) throws Exception {
		String inputFile = "./tests/evaluation/" + fileName;

		SimpleLogger.logger().log("Input: " + inputFile, SimpleLogger.LogLevel.Level.DEBUG);
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./facts " + inputFile);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output       -F ./facts  " + inputFile);
		Util.doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithEDB")
	@ParameterizedTest(name = "Evaluation Tests Valid WithEDB")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in"})
	void evaluationTestsBottomUpNaiveCompareSouffleWithEDBs(String fileName) throws Exception {
		String outname = FileUtil.changeExtension(fileName, "_with_edb.dl");
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withedbs/ "
						+ " ./tests/evaluation/withedbs/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output         -F ./tests/evaluation/withedbs/ ./tests/evaluation/withedbs/"
						+ fileName);
		Util.doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithNEG")
	@ParameterizedTest(name = "Evaluation Tests Valid WithNEG")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in" })
	void evaluationTestsBottomUpNaiveCompareSouffleWithNEGs(String fileName) throws Exception {
		String outname = FileUtil.changeExtension(fileName, "_with_neg.dl");
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withneg/ "
				+ " ./tests/evaluation/withneg/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output         -F ./tests/evaluation/withneg/ ./tests/evaluation/withneg/"
						+ fileName);
		Util.doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithBinPred")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in","evalTest_3.in","evalTest_4.in","evalTest_5.in","evalTest_6.in", "evalTest_7.in","evalTest_8.in","evalTest_9.in" })
	void evaluationTestsBottomUpNaiveCompareWithBinPred(String fileName) throws Exception {
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output        -F ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		Util.doEvaluationTest(d1, d2);
	}

    @DisplayName("Compare Internal Evaluation to Souffle WithMeta")
    @ParameterizedTest
	@ValueSource(strings = {/* "evalTest_1.in", */ "evalTest_2.in", "evalTest_3.in" })
    void evaluationTestsBottomUpNaiveCompareWithMeta(String fileName) throws Exception {
        CmdLineOpts d1 = FileUtil.parseDescription(
                "-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        CmdLineOpts d2 = FileUtil.parseDescription(
                "-e metadl -D ./tests/output        -F ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        Util.doEvaluationTest(d1, d2);
    }

	@DisplayName("Compare Internal Evaluation to Souffle WithWildcard")
	@ParameterizedTest
	@ValueSource(strings = {"evalTest_1.in", "evalTest_2.in"})
	void evaluationTestsBottomUpNaiveCompareWithWildcard(String fileName) throws Exception {
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output        -F ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		Util.doEvaluationTest(d1, d2);
	}

	static Stream<String> metadlPatternTests() {
		String[] tests = { "evalTest_2:metadl", "evalTest_3:metadl", "evalTest_4:metadl",
						   "evalTest_5:metadl", "evalTest_6:metadl", "evalTest_7:metadl",
						   // "evalTest_8", TODO: re-enable once MetaDL uses DatalogProjection2
						   "evalTest_9:metadl",
						   // "evalTest_10", TODO: re-enable once MetaDL uses DatalogProjection2
						   "evalTest_11:metadl", "evalTest_12:java", "evalTest_13:java",
						   "evalTest_14:metadl", "evalTest_15:java",
						   // "evalTest_16", Disabled, due to introduction of inexact matches
						   // "evalTest_17", Disabled, due to stricter type checking
						   // TODO: re-enable evalTest_17 once conversion functors node -> int and int -> node
						   // are introduced
						   "evalTest_18:java"
		};
		return Arrays.stream(tests);
	}

	@DisplayName("Evaluate programs containing patterns using Souffle")
	@ParameterizedTest
	@MethodSource("metadlPatternTests")
 	void evaluationTestPatternsSouffle(String testDesc) throws Exception {
		String fileName = Util.parseTestDescription(testDesc)[0];
		String lang = Util.parseTestDescription(testDesc)[1];

		IOFileFilter ff = new WildcardFileFilter(fileName + "_input*");
		Iterator<File> analyzedFileIt = FileUtils.iterateFiles(new File("./tests/evaluation/withimport"), ff, null);

		Util.singleEvaluationTest("./tests/output/souffle",
							 "./tests/evaluation/withimport/facts",
							 analyzedFileIt.hasNext() ? Collections.singletonList(analyzedFileIt.next()) : Collections.emptyList(),
							 "./tests/evaluation/withimport",
							 "./tests/evaluation/withimport/expected",
							 fileName,
							 ".in",
							 lang,
							 "-e souffle");
	}

	@DisplayName("Evaluate programs containing patterns using internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlPatternTests")
	void evaluationTestPatternsInternal(String testDesc) throws Exception {
		String fileName = Util.parseTestDescription(testDesc)[0];
		String lang = Util.parseTestDescription(testDesc)[1];

		IOFileFilter ff = new WildcardFileFilter(fileName + "_input*");
		Iterator<File> analyzedFileIt = FileUtils.iterateFiles(new File("./tests/evaluation/withimport"), ff, null);

		Util.singleEvaluationTest("./tests/output/souffle",
							 "./tests/evaluation/withimport/facts",
							 analyzedFileIt.hasNext() ? Collections.singletonList(analyzedFileIt.next()) : Collections.emptyList(),
							 "./tests/evaluation/withimport",
							 "./tests/evaluation/withimport/expected",
							 fileName,
							 ".in",
							 lang,
							 "-e metadl");
	}

	@DisplayName("Evaluate programs containing patterns using internal parallel evaluator")
	@ParameterizedTest
	@MethodSource("metadlPatternTests")
	void evaluationTestPatternsInternalParallel(String testDesc) throws Exception {
		String fileName = Util.parseTestDescription(testDesc)[0];
		String lang = Util.parseTestDescription(testDesc)[1];

		IOFileFilter ff = new WildcardFileFilter(fileName + "_input*");
		Iterator<File> analyzedFileIt = FileUtils.iterateFiles(new File("./tests/evaluation/withimport"), ff, null);

		Util.singleEvaluationTest("./tests/output/souffle",
							 "./tests/evaluation/withimport/facts",
							 analyzedFileIt.hasNext() ? Collections.singletonList(analyzedFileIt.next()) : Collections.emptyList(),
							 "./tests/evaluation/withimport",
							 "./tests/evaluation/withimport/expected",
							 fileName,
							 ".in",
							 lang,
							 "-e metadl-par");
	}
}
