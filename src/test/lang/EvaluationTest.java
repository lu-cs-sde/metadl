package lang;
import java.io.File;
import java.util.ArrayList;

import lang.ast.Program;
import lang.evaluation.Evaluation;

public class EvaluationTest {

    /*
    * Compare one evaluation scheme against another.
    */
	public void doEvaluationTest(EvaluationTarget et) throws Exception {
		Program program1 = (Program) Util.parse(new File(et.progDir, et.progFileName));
        et.ed1.evaluation.evaluate(program1);

		Program program2 = (Program) Util.parse(new File(et.progDir, et.progFileName));
        et.ed2.evaluation.evaluate(program2);

        et.forEach(outputName -> {
            Util.compareOutputFiles( new File(et.ed1.outputDir, outputName),
                                     new File(et.ed2.outputDir, outputName));
        });
    }

    public static class EvaluationTarget extends ArrayList<String> {
        public final EvaluationDescription ed1;
        public final EvaluationDescription ed2;

        public final String progFileName;
        public final File progDir;

        public EvaluationTarget(String progFileName, File progDir, EvaluationDescription ed1, EvaluationDescription ed2) {
            this.progFileName = progFileName;
            this.progDir = progDir;
            this.ed1 = ed1;
            this.ed2 = ed2;
        }
    }

    public static class EvaluationDescription {
        public final File outputDir;
        public final Evaluation evaluation;

        public EvaluationDescription(File outputDir, Evaluation evaluation) {

            this.outputDir = outputDir;
            this.evaluation = evaluation;
        }
    }
}
