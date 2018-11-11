package lang;

import lang.ast.ExtensionalDB;
import lang.ast.Program;

public abstract class InternalEvaluation extends Evaluation {
	
	protected void loadEBDFacts(Program p) {
		// System.out.println("Loading EDB Facts: ");
		//
		// StringBuilder sb = new StringBuilder();
		// p.collectMetaInfo(sb);
		// System.err.println(sb.toString());
		//
		// p.edbPredicates().forEach(pred -> System.out.println(pred.getID() +
		//         " at file: " + ((ExtensionalDB)pred.definingStmt()).getSTRING()));
	}
}
