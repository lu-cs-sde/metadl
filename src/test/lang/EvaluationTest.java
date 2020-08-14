package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Ignore;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import eval.Relation2;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.PredicateSymbol;
import lang.ast.PredicateType;
import lang.ast.config.Description;
import lang.ast.Literal;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.PseudoTuple;
import lang.relation.Relation;
import lang.relation.RelationWrapper;

public class EvaluationTest {
	/*
	 * Compare one evaluation scheme against another.
	 */
	public Map<String, RelationWrapper> doSingleEvaluation(Description d1) throws Exception {
		Program program1 = d1.getTask().perform();
		FormalPredicate fpOut1 = program1.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);

		if (fpOut1 == null)
			fail();
		HashMap<String, RelationWrapper> nameToRel = new HashMap<>();
		for (PredicateSymbol psym : fpOut1.predicates()) {
			Literal output = psym.parentLiteral();
			PredicateRef pr = (PredicateRef)output.getTerms(0);
			FormalPredicate fp = pr.formalpredicate();

			File in1 = new File(d1.outputDir() + "/" + fp.predicateName() + ".csv");

			Relation2 r1 = new Relation2(fp.realArity(), fp.getPRED_ID());
			CSVUtil.readRelation(program1.evalCtx(), fp.type(), r1, d1.outputDir() + "/" + fp.predicateName() + ".csv");

			SimpleLogger.logger().log("Read relation from: " + in1.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
			nameToRel.put(fp.predicateName(), new RelationWrapper(program1.evalCtx(), r1, fp.type()));
		}
		return nameToRel;
	}

	public void doEvaluationTest(Description d1, Description d2) throws Exception {
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

		Map<String, RelationWrapper> outRelations = doSingleEvaluation(d1);
		for (Map.Entry<String, RelationWrapper> rel : outRelations.entrySet()) {
			String expectedF = "./tests/evaluation/withimport/expected/" + fileName + "/" + rel.getKey() + ".csv";
			Relation2 expectedRel = new Relation2(rel.getValue().getRelation().arity(),
												  rel.getValue().getRelation().getName());

			CSVUtil.readRelation(rel.getValue().getContext(),
								 rel.getValue().type(),
								 expectedRel,
								 expectedF);

			if (!expectedRel.equals(rel.getValue().getRelation())) {
				System.out.println(rel.getKey() + " expects ");
				System.out.println(expectedRel.tuples());
				System.out.println(rel.getKey() + " actual ");
				System.out.println(rel.getValue().getRelation().tuples());

			}
			String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
			assertEquals(expectedRel, rel.getValue().getRelation(), msg);
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

		Map<String, RelationWrapper> outRelations = doSingleEvaluation(d1);
		for (Map.Entry<String, RelationWrapper> rel : outRelations.entrySet()) {
			String expectedF = "./tests/evaluation/metadl-java/expected/" + fileName + "/" + rel.getKey() + ".csv";
			Relation2 expectedRel = new Relation2(rel.getValue().getRelation().arity(),
												  rel.getValue().getRelation().getName());

			CSVUtil.readRelation(rel.getValue().getContext(),
								 rel.getValue().type(),
								 expectedRel,
								 expectedF);

			if (!expectedRel.equals(rel.getValue().getRelation())) {
				System.out.println(rel.getKey() + " expects ");
				System.out.println(expectedRel.tuples());
				System.out.println(rel.getKey() + " actual ");
				System.out.println(rel.getValue().getRelation().tuples());

			}
			String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
			assertEquals(expectedRel, rel.getValue().getRelation(), msg);
		}
	}

}
