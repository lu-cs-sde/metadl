package lang;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import lang.ast.Program;
import lang.ast.config.Description;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.relation.Relation;

public class EvaluationTest {
	/*
	 * Compare one evaluation scheme against another.
	 */
	public void doEvaluationTest(Description d1, Description d2) throws Exception {
		Program program1 = (Program) FileUtil.parse(new File(d1.getInput().getPath()));
		d1.evaluationMethod().evaluate(program1, d1);

		Program program2 = (Program) FileUtil.parse(new File(d2.getInput().getPath()));
		d2.evaluationMethod().evaluate(program2, d2);

		program1.getFormalPredicates().forEach(fp -> {
			File in1 = new File(d1.outputDir() + "/" + fp.predicateName() + ".csv");
			Relation r1 = CSVUtil.readRelationFrom(in1, fp);
			if (r1 == null)
				assertTrue(false);

			File in2 = new File(d2.outputDir() + "/" + fp.predicateName() + ".csv");
			Relation r2 = CSVUtil.readRelationFrom(in2, fp);
			if (r2 == null)
				assertTrue(false);

			assertTrue(r1.equals(r2));
		});
	}

	@DisplayName("Compare Internal Evaluation to Souffle")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "evalTest_1.in", "evalTest_2.in", "evalTest_3.in", "evalTest_4.in", "evalTest_5.in",
			"evalTest_6.in", "evalTest_7.in", "evalTest_8.in", "evalTest_9.in"})
	void evaluationTestsBottomUpNaiveCompareSouffle(String fileName) throws Exception {
		Description d1 = FileUtil.parseDescription(
				"eval::souffle      -OUT ./tests/evaluation/bottomupout/souffle ./tests/evaluation/" + fileName);
		Description d2 = FileUtil.parseDescription(
				"eval::bottomupnaive -OUT ./tests/evaluation/bottomupout         ./tests/evaluation/" + fileName);
		doEvaluationTest(d1, d2);
	}
	/*
	 * @DisplayName("Compare Internal Evaluation to Souffle")
	 * 
	 * @ParameterizedTest(name = "Evaluation Tests Valid")
	 * 
	 * @ValueSource(strings = { "evalTest_1.in", "evalTest_2.in"}) void
	 * evaluationTestsBottomUpNaiveCompareSouffleWithEDBs(String fileName) throws
	 * Exception { String outname = FileUtil.changeExtension(fileName,
	 * "_with_edb.dl"); Description d1 = FileUtil.
	 * parseDescription("external::souffle      -OUT ./tests/evaluation/bottomupout/souffle -FACTS ./tests/evaluation/withedbs/ -SOUFFLEOUT "
	 * + outname + " ./tests/evaluation/withedbs/" + fileName); Description d2 =
	 * FileUtil.
	 * parseDescription("internal::bottomupnaive -OUT ./tests/evaluation/bottomupout         -FACTS ./tests/evaluation/withedbs/ ./tests/evaluation/withedbs/"
	 * + fileName); doEvaluationTest(d1, d2); }
	 */
}
