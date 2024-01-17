package lang;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.WildcardFileFilter;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import lang.io.FileUtil;
import lang.io.SimpleLogger;

public class EvaluationTest {
	@DisplayName("Compare Internal Evaluation to Souffle")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = {"evalTest_1.in",
                          "evalTest_2.in",
                          "evalTest_3.in",
                          "evalTest_4.in",
                          "evalTest_5.in",
                          "evalTest_6.in",
                          "evalTest_7.in",
                          "evalTest_8.in",
                          "evalTest_9.in",
                          //"evalTest_10.in",
                          //"evalTest_11.in",
                          //"evalTest_12.in",
                          "evalTest_13.in",
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

}
