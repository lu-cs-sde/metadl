package lang.evaluation;

import java.io.File;

import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.config.Description;
import lang.io.CSVUtil;

public abstract class InternalEvaluation extends Evaluation {

	public void loadEBDFacts(Program program, Description descr) {
//		program.objects.addAll(program.uniqueFileObjects());
//
//		SimpleLogger.logger().log("Load EDB Facts", SimpleLogger.LogLevel.Level.DEBUG);
//		List<FormalPredicate> spreds = program.getFormalPredicates();
//		spreds.forEach(sp -> {
//			sp.relation = new Relation(sp.fileRelation());
//
//			sp.edbPredicates().forEach(ps -> {
//				ExtensionalDB edb = (ExtensionalDB) ps.literal().stmt();
//				String fn = edb.getFileLocation().getSTRING();
//				File f = new File(fn);
//				
//			    if(f.getName().equals(fn)) {
//			    	f = new File(descr.factsDir() + "/" + f.getName());
//			    }
//			    
//				if (!f.exists()) {
//					SimpleLogger.logger().log("Missing EDB File: " + f.getAbsolutePath(), SimpleLogger.LogLevel.Level.ERROR);
//					System.exit(0);
//				}
//
//				SimpleLogger.logger().log("Read in: " + f.getAbsolutePath() + " into relation: " + sp.predicateName(), SimpleLogger.LogLevel.Level.DEBUG);
//				CSVUtil.readFileInto(program, sp, f);
//			});
//		});
	}
	
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
			FormalPredicate fp = ref.formalpredicate();
			if(fp.relation != null)
				CSVUtil.dumpFileInto(fp, new File(descr.outputDir() + "/" + fp.predicateName() + ".csv"));
		});
	}
}
