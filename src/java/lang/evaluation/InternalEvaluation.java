package lang.evaluation;

import java.io.File;

import lang.ast.ExtensionalDB;
import lang.ast.List;
import lang.ast.Program;
import lang.ast.FormalPredicate;
import lang.io.CSVUtil;
import lang.io.SimpleLogger;
import lang.relation.Relation;

public abstract class InternalEvaluation extends Evaluation {

	protected void loadEBDFacts(Program program) {
		program.objects.addAll(program.uniqueFileObjects());

		SimpleLogger.logger().log("Load EDB Facts", SimpleLogger.LogLevel.Level.DEBUG);
		List<FormalPredicate> spreds = program.getFormalPredicates();
		spreds.forEach(sp -> {
			sp.relation = new Relation(sp.fileRelation());

			sp.edbPredicates().forEach(ps -> {
				ExtensionalDB edb = (ExtensionalDB) ps.literal().stmt();
				String fn = edb.getFileLocation().getSTRING();
				File f = new File(fn);

				if (!f.exists()) {
					SimpleLogger.logger().log("Missing EDB File: " + fn, SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}

				SimpleLogger.logger().log("Read in: " + fn + " into relation: " + sp.predicateName(), SimpleLogger.LogLevel.Level.DEBUG);
				CSVUtil.readFileInto(program, sp, f);
			});
		});
	}
}
