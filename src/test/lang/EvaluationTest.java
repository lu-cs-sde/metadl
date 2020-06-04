package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.PredicateSymbol;
import lang.ast.OUTPUTLiteral;
import lang.ast.config.Description;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.PseudoTuple;
import lang.relation.Relation;

public class EvaluationTest {
	/*
	 * Compare one evaluation scheme against another.
	 */
	public Map<String, Relation> doSingleEvaluation(Description d1) throws Exception {
		Program program1 = d1.getTask().perform();
		FormalPredicate fpOut1 = program1.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);

		if (fpOut1 == null)
			fail();
		HashMap<String, Relation> nameToRel = new HashMap<>();
		for (PredicateSymbol psym : fpOut1.predicates()) {
			OUTPUTLiteral output = (OUTPUTLiteral) psym.literal();
			PredicateRef pr = (PredicateRef)output.getTerm();
			FormalPredicate fp = pr.formalpredicate();

			File in1 = new File(d1.outputDir() + "/" + fp.predicateName() + ".csv");

			Relation r1 = CSVUtil.readRelationFrom(program1, in1, fp.realArity());

			SimpleLogger.logger().log("Read relation from: " + in1.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
			if (r1 == null) {
				SimpleLogger.logger().log("Relation is null: " + in1.getPath(), SimpleLogger.LogLevel.Level.ERROR);
				assertTrue(false);
			}
			nameToRel.put(fp.predicateName(), r1);
		}
		return nameToRel;
	}

	public void doEvaluationTest(Description d1, Description d2) throws Exception {
		Map<String, Relation> rels1 = doSingleEvaluation(d1);
		Map<String, Relation> rels2 = doSingleEvaluation(d2);

		for (Map.Entry<String, Relation> sr : rels1.entrySet()) {
			Relation rel1 = sr.getValue();
			Relation rel2 = rels2.get(sr.getKey());
			assertTrue(rel2 != null);
			assertEquals(rel1, rel2, "Relation " + sr.getKey() + " differs.");
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
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/output/souffle -FACTS ./facts " + inputFile);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/output       -FACTS ./facts  " + inputFile);
		doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithEDB")
	@ParameterizedTest(name = "Evaluation Tests Valid WithEDB")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in"})
	void evaluationTestsBottomUpNaiveCompareSouffleWithEDBs(String fileName) throws Exception {
		String outname = FileUtil.changeExtension(fileName, "_with_edb.dl");
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withedbs/ -SOUFFLEOUT "
						+ outname + " ./tests/evaluation/withedbs/" + fileName);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/output         -FACTS ./tests/evaluation/withedbs/ ./tests/evaluation/withedbs/"
						+ fileName);
		doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithNEG")
	@ParameterizedTest(name = "Evaluation Tests Valid WithNEG")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in" })
	void evaluationTestsBottomUpNaiveCompareSouffleWithNEGs(String fileName) throws Exception {
		String outname = FileUtil.changeExtension(fileName, "_with_neg.dl");
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withneg/ -SOUFFLEOUT "
						+ outname + " ./tests/evaluation/withneg/" + fileName);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/output         -FACTS ./tests/evaluation/withneg/ ./tests/evaluation/withneg/"
						+ fileName);
		doEvaluationTest(d1, d2);
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithBinPred")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in","evalTest_3.in","evalTest_4.in","evalTest_5.in","evalTest_6.in", "evalTest_7.in","evalTest_8.in","evalTest_9.in" })
	void evaluationTestsBottomUpNaiveCompareWithBinPred(String fileName) throws Exception {
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/output        -FACTS ./tests/evaluation/withbinpred/ ./tests/evaluation/withbinpred/" + fileName);
		doEvaluationTest(d1, d2);
	}

    @DisplayName("Compare Internal Evaluation to Souffle WithMeta")
    @ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = {/* "evalTest_1.in", */ "evalTest_2.in", "evalTest_3.in" })
    void evaluationTestsBottomUpNaiveCompareWithMeta(String fileName) throws Exception {
        Description d1 = FileUtil.parseDescription(
                "eval::souffle      -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        Description d2 = FileUtil.parseDescription(
                "eval::bottomupnaive -OUT ./tests/output        -FACTS ./tests/evaluation/withmeta/facts ./tests/evaluation/withmeta/" + fileName);
        doEvaluationTest(d1, d2);
    }

	@DisplayName("Compare Internal Evaluation to Souffle WithWildcard")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = {"evalTest_1.in", "evalTest_2.in"})
	void evaluationTestsBottomUpNaiveCompareWithWildcard(String fileName) throws Exception {
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/output        -FACTS ./tests/evaluation/withwildcard/facts ./tests/evaluation/withwildcard/" + fileName);
		doEvaluationTest(d1, d2);
	}

	@DisplayName("Evaluate programs containing patterns using Souffle")
	@ParameterizedTest(name = "Pattern Tests")
	@ValueSource(strings = { "evalTest_2", "evalTest_3", "evalTest_4",
							 "evalTest_5", "evalTest_6", "evalTest_7",
							 "evalTest_8", "evalTest_9", "evalTest_10",
							 "evalTest_11", "evalTest_12", "evalTest_13",
							 "evalTest_14", "evalTest_15",
							 // "evalTest_16", Disabled, due to introduction of inexact matches
							 "evalTest_17", "evalTest_18"})
	void evaluationTestSoufflePatterns(String fileName) throws Exception {
		Description d1 = FileUtil.parseDescription(
		   "eval::souffle -OUT ./tests/output/souffle -FACTS ./tests/evaluation/withimport/facts ./tests/evaluation/withimport/"
		   + fileName + ".in");

		Map<String, Relation> outRelations = doSingleEvaluation(d1);
		for (Map.Entry<String, Relation> rel : outRelations.entrySet()) {
			File expectedF = new File ("./tests/evaluation/withimport/expected/" + fileName + "/" + rel.getKey() + ".csv");
			Relation expectedRel = CSVUtil.readRelationFrom(null, expectedF, rel.getValue().arity());
			if (!expectedRel.equals(rel.getValue())) {
				System.out.println(rel.getKey() + " expects ");
				System.out.println(expectedRel);
				System.out.println(rel.getKey() + " actual ");
				System.out.println(rel.getValue());

			}
			String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
			assertEquals(expectedRel, rel.getValue(), msg);
		}
	}


	@DisplayName("Evaluate MetaDL-Java programs")
	@ParameterizedTest(name = "MetaDL-Java programs")
	@ValueSource(strings = {"bad-covariant-equals", "switch-no-default", "clone-idioms", "number-ctor",
							"unwritten-field", "naming-convention", "reference-to-mutable-object", "missing-override",
							"reference-equality", "boxed-primitive-constructor", "operator-precedence",
							"type-param-unused-in-formals", "paper-examples", "java7", "java8"})
	void evaluationTestMetaDLJava(String fileName) throws Exception {
		Description d1 = FileUtil.parseDescription(
		   "eval::souffle -OUT ./tests/output/souffle -FACTS ./tests/evaluation/metadl-java/facts ./tests/evaluation/metadl-java/"
		   + fileName + ".mdl");

		Map<String, Relation> outRelations = doSingleEvaluation(d1);
		for (Map.Entry<String, Relation> rel : outRelations.entrySet()) {
			File expectedF = new File ("./tests/evaluation/metadl-java/expected/" + fileName + "/" + rel.getKey() + ".csv");
			Relation expectedRel = CSVUtil.readRelationFrom(null, expectedF, rel.getValue().arity());
			if (!expectedRel.equals(rel.getValue())) {
				System.out.println(rel.getKey() + " expects ");
				System.out.println(expectedRel);
				System.out.println(rel.getKey() + " actual ");
				System.out.println(rel.getValue());

			}
			String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
			assertEquals(expectedRel, rel.getValue(), msg);
		}
	}

}
