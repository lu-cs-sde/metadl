package lang.evaluation;

import java.io.File;

import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.config.Description;
import lang.io.CSVUtil;

public abstract class InternalEvaluation extends Evaluation {

	public void dumpAllRelations(Program program, Description descr) {
		program.getFormalPredicates().forEach(fp -> {
			if(fp.literal().isInclusive())
				CSVUtil.dumpFileInto(fp, new File(descr.outputDir() + "/" + fp.predicateName() + ".csv"));
		});
	}

	public void dumpRelations(Program program, Description descr) {
		FormalPredicate output = program.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);
		if(output == null) return;

		output.relation.tuples().forEach(ps -> {
			PredicateRef ref = (PredicateRef) ps.coord(0);
			FormalPredicate fp = program.formalPredicateMap().get(ref.getPRED_ID());
			if (fp != null && fp.relation != null && descr != null)
				CSVUtil.dumpFileInto(fp, new File(descr.outputDir() + "/" + fp.predicateName() + ".csv"));
		});
	}
}
