package lang.evaluation;

import java.io.File;

import lang.ast.ExtensionalDB;
import lang.ast.List;
import lang.ast.Program;
import lang.ast.config.Description;
import lang.ast.FormalPredicate;
import lang.io.CSVUtil;
import lang.io.SimpleLogger;
import lang.relation.Relation;

public abstract class InternalEvaluation extends Evaluation {

	public void loadEBDFacts(Program program, Description descr) {
		program.objects.addAll(program.uniqueFileObjects());

		SimpleLogger.logger().log("Load EDB Facts", SimpleLogger.LogLevel.Level.DEBUG);
		List<FormalPredicate> spreds = program.getFormalPredicates();
		spreds.forEach(sp -> {
			sp.relation = new Relation(sp.fileRelation());

			sp.edbPredicates().forEach(ps -> {
				ExtensionalDB edb = (ExtensionalDB) ps.literal().stmt();
				String fn = edb.getFileLocation().getSTRING();
				File f = new File(fn);
				
			    if(f.getName().equals(fn)) {
			    	f = new File(descr.factsDir() + "/" + f.getName());
			    }
			    
				if (!f.exists()) {
					SimpleLogger.logger().log("Missing EDB File: " + f.getAbsolutePath(), SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}

				SimpleLogger.logger().log("Read in: " + f.getAbsolutePath() + " into relation: " + sp.predicateName(), SimpleLogger.LogLevel.Level.DEBUG);
				CSVUtil.readFileInto(program, sp, f);
			});
		});
	}
}
