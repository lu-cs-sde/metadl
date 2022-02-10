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
	public Map<String, RelationWrapper> doSingleEvaluation(CmdLineOpts d1) throws Exception {
		Program program1 = Compiler.run(d1);
		FormalPredicate fpOut1 = program1.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);

		if (fpOut1 == null)
			fail();
		HashMap<String, RelationWrapper> nameToRel = new HashMap<>();

		RelationWrapper outputTuples = new RelationWrapper(program1.evalCtx(), fpOut1.relation2(), fpOut1.type());

		for (RelationWrapper.TupleWrapper t : outputTuples.tuples()) {
			String pred = t.getAsString(0);
			FormalPredicate fp = program1.formalPredicateMap().get(pred);

			File in1 = new File(d1.getOutputDir() + "/" + fp.predicateName() + ".csv");

			Relation2 r1 = new Relation2(fp.realArity(), fp.getPRED_ID());
			CSVUtil.readRelation(program1.evalCtx(), fp.type(), r1, d1.getOutputDir() + "/" + fp.predicateName() + ".csv");

			SimpleLogger.logger().log("Read relation from: " + in1.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
			nameToRel.put(fp.predicateName(), new RelationWrapper(program1.evalCtx(), r1, fp.type()));
		}
		return nameToRel;
	}

	public void doEvaluationTest(CmdLineOpts d1, CmdLineOpts d2) throws Exception {
		Map<String, RelationWrapper> rels1 = doSingleEvaluation(d1);
		Map<String, RelationWrapper> rels2 = doSingleEvaluation(d2);

		for (Map.Entry<String, RelationWrapper> sr : rels1.entrySet()) {
			RelationWrapper rel1 = sr.getValue();
			RelationWrapper rel2 = rels2.get(sr.getKey());
			assertTrue(rel2 != null);
			assertEquals(rel1.tuples(), rel2.tuples(), "Relation " + sr.getKey() + " differs.");
		}

		// so far we proved rels1 is include in rels2, check their sizes are equal
		assertEquals(rels1.size(), rels2.size());
	}

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
		doEvaluationTest(d1, d2);
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
		doEvaluationTest(d1, d2);
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
		doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithBinPred")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in","evalTest_3.in","evalTest_4.in","evalTest_5.in","evalTest_6.in", "evalTest_7.in","evalTest_8.in","evalTest_9.in" })
	void evaluationTestsBottomUpNaiveCompareWithBinPred(String fileName) throws Exception {
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output        -F ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		doEvaluationTest(d1, d2);
	}

    @DisplayName("Compare Internal Evaluation to Souffle WithMeta")
    @ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = {/* "evalTest_1.in", */ "evalTest_2.in", "evalTest_3.in" })
    void evaluationTestsBottomUpNaiveCompareWithMeta(String fileName) throws Exception {
        CmdLineOpts d1 = FileUtil.parseDescription(
                "-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        CmdLineOpts d2 = FileUtil.parseDescription(
                "-e metadl -D ./tests/output        -F ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        doEvaluationTest(d1, d2);
    }

	@DisplayName("Compare Internal Evaluation to Souffle WithWildcard")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = {"evalTest_1.in", "evalTest_2.in"})
	void evaluationTestsBottomUpNaiveCompareWithWildcard(String fileName) throws Exception {
		CmdLineOpts d1 = FileUtil.parseDescription(
				"-e souffle      -D ./tests/output/souffle -F ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		CmdLineOpts d2 = FileUtil.parseDescription(
				"-e metadl -D ./tests/output        -F ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		doEvaluationTest(d1, d2);
	}

	static Stream<String> metadlPatternTests() {
		String[] tests = { "evalTest_2", "evalTest_3", "evalTest_4",
						   "evalTest_5", "evalTest_6", "evalTest_7",
						   // "evalTest_8", TODO: re-enable once MetaDL uses DatalogProjection2
						   "evalTest_9",
						   // "evalTest_10", TODO: re-enable once MetaDL uses DatalogProjection2
						   "evalTest_11", "evalTest_12", "evalTest_13",
						   "evalTest_14", "evalTest_15",
						   // "evalTest_16", Disabled, due to introduction of inexact matches
						   // "evalTest_17", Disabled, due to stricter type checking
						   // TODO: re-enable evalTest_17 once conversion functors node -> int and int -> node
						   // are introduced
						   "evalTest_18"
		};
		return Arrays.stream(tests);
	}

	@DisplayName("Evaluate programs containing patterns using Souffle")
	@ParameterizedTest
	@MethodSource("metadlPatternTests")
 	void evaluationTestPatternsSouffle(String fileName) throws Exception {
		IOFileFilter ff = new WildcardFileFilter(fileName + "_input*");
		Iterator<File> analyzedFileIt = FileUtils.iterateFiles(new File("./tests/evaluation/withimport"), ff, null);

		singleEvaluationTest("./tests/output/souffle",
							 "./tests/evaluation/withimport/facts",
							 analyzedFileIt.hasNext() ? Collections.singletonList(analyzedFileIt.next()) : Collections.emptyList(),
							 "./tests/evaluation/withimport",
							 "./tests/evaluation/withimport/expected",
							 fileName,
							 ".in",
							 "-e souffle");
	}

	@DisplayName("Evaluate programs containing patterns using internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlPatternTests")
	void evaluationTestPatternsInternal(String fileName) throws Exception {
		IOFileFilter ff = new WildcardFileFilter(fileName + "_input*");
		Iterator<File> analyzedFileIt = FileUtils.iterateFiles(new File("./tests/evaluation/withimport"), ff, null);

		singleEvaluationTest("./tests/output/souffle",
							 "./tests/evaluation/withimport/facts",
							 analyzedFileIt.hasNext() ? Collections.singletonList(analyzedFileIt.next()) : Collections.emptyList(),
							 "./tests/evaluation/withimport",
							 "./tests/evaluation/withimport/expected",
							 fileName,
							 ".in",
							 "-e metadl");
	}

	// @DisplayName("Evaluate programs containing patterns using the parallel internal evaluator")
	// @ParameterizedTest
	// @MethodSource("metadlPatternTests")
	// void evaluationTestPatternsInternalParallel(String fileName) throws Exception {
	// 	singleEvaluationTest("./tests/output/souffle",
	// 						 "./tests/evaluation/withimport/facts",
	// 						 "./tests/evaluation/withimport",
	// 						 "./tests/evaluation/withimport/expected",
	// 						 fileName,
	// 						 ".in",
	// 						 "-e metadl-par");
	// }

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

	void singleEvaluationTest(String outputDir, String factDir,
							  List<File>  analyzedFiles,
							  String srcDir, String expectedDir,
							  String fileName, String fileExt,
							  String cmd) throws Exception {

		String sources = analyzedFiles.stream().map(f -> f.getPath() + ",A").collect(Collectors.joining(":"));

		CmdLineOpts d1 = FileUtil.parseDescription(
		   cmd
		   + " -D " + outputDir
		   + " -F " + factDir
		   + (sources.isEmpty() ? " " : " -S ") + sources
		   + " " + srcDir + "/"
		   + fileName + fileExt);

		Map<String, RelationWrapper> outRelations = doSingleEvaluation(d1);
		for (Map.Entry<String, RelationWrapper> rel : outRelations.entrySet()) {
			String expectedF = expectedDir + "/" + fileName + "/" + rel.getKey() + ".csv";
			Relation2 expectedRel = new Relation2(rel.getValue().getRelation().arity(),
												  rel.getValue().getRelation().getName());

			CSVUtil.readRelation(rel.getValue().getContext(),
								 rel.getValue().type(),
								 expectedRel,
								 expectedF);

			RelationWrapper expectedRelWrapper = new RelationWrapper(rel.getValue().getContext(),
																	 expectedRel,
																	 rel.getValue().type());

			// if (!expectedRel.equals(rel.getValue().getRelation())) {
			// 	System.out.println(rel.getKey() + " expects ");
			// 	System.out.println(expectedRelWrapper.tuples());
			// 	System.out.println(rel.getKey() + " actual ");
			// 	System.out.println(rel.getValue().tuples());

			// }
			Util.printSimmetricDifference(expectedRelWrapper, rel.getValue());

			String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
			assertEquals(expectedRel, rel.getValue().getRelation(), msg);
		}
	}


	@DisplayName("Evaluate MetaDL-Java programs with Souffle")
	@ParameterizedTest
	@MethodSource("metadlJavaTests")
	void evaluationTestMetaDLJavaSouffle(String fileName) throws Exception {
		String analyzedFilesDir = "tests/evaluation/metadl-java/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.java");

		singleEvaluationTest("tests/output/souffle",
							 "tests/evaluation/metadl-java/facts",
							 analyzedFiles,
							 "tests/evaluation/metadl-java",
							 "tests/evaluation/metadl-java/expected",
							 fileName,
							 ".mdl",
							 "-e souffle");
	}

	@DisplayName("Evaluate MetaDL-Java programs with the internal evaluator")
	@ParameterizedTest
	@MethodSource("metadlJavaTests")
	void evaluationTestMetaDLJavaInternal(String fileName) throws Exception {
		String analyzedFilesDir = "tests/evaluation/metadl-java/src";
		List<File> analyzedFiles = FileUtil.flattenFilesAndDirs(Collections.singletonList(new File(analyzedFilesDir, fileName)), "*.java");

		singleEvaluationTest("tests/output/",
							 "tests/evaluation/metadl-java/facts",
							 analyzedFiles,
							 "tests/evaluation/metadl-java",
							 "tests/evaluation/metadl-java/expected",
							 fileName,
							 ".mdl",
							 "-e metadl");
	}

	// @DisplayName("Evaluate MetaDL-Java programs with the parallel internal evaluator")
	// @ParameterizedTest
	// @MethodSource("metadlJavaTests")
	// void evaluationTestMetaDLJavaInternalParallel(String fileName) throws Exception {
	// 	singleEvaluationTest("./tests/output/",
	// 						 "./tests/evaluation/metadl-java/facts",
	// 						 "./tests/evaluation/metadl-java",
	// 						 "./tests/evaluation/metadl-java/expected",
	// 						 fileName,
	// 						 ".mdl",
	// 						 "-e metadl");
	// }

}
