package lang;

import lang.ast.Program;

public class BacktrackingEvaluation extends InternalEvaluation{
	@Override
	void evaluate(Program p) {
		System.out.println("Evaluate Backtracking");
		loadEBDFacts(p);
	}
}