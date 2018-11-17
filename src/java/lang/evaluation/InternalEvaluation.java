package lang.evaluation;

import java.io.File;

import lang.ast.ExtensionalDB;
import lang.ast.List;
import lang.ast.Program;
import lang.ast.SuperPredicate;
import lang.io.CSVUtil;
import lang.relation.Relation;

public abstract class InternalEvaluation extends Evaluation {

	protected void loadEBDFacts(Program program) {
		program.objects.addAll(program.uniqueFileObjects());

		System.out.println("Load EDB Facts");
		List<SuperPredicate> spreds = program.getSuperPredicateList();
		spreds.forEach(sp -> {
			sp.relation = new Relation(sp.fileRelation());

			sp.edbPredicates().forEach(ps -> {
				ExtensionalDB edb = (ExtensionalDB) ps.literal().stmt();
				String fn = edb.getFileLocation().getSTRING();
				File f = new File(fn);

				if (!f.exists()) {
					System.err.println("Missing EDB File: " + fn);
					System.exit(0);
				}

				System.out.println("Read in: " + fn + " into relation: " + sp.predicateName());
				CSVUtil.readFileInto(program, sp, f);
			});
		});
	}
}
