package lang;

import java.util.HashSet;

import lang.ast.Constant;
import lang.ast.Program;
import lang.ast.Rule;
import lang.ast.SuperPredicate;

public class BacktrackingEvaluation extends InternalEvaluation {
	@Override
	void evaluate(Program p) {
		System.out.println("Evaluate Backtracking");
		loadEBDFacts(p);
		p.getSuperPredicates().forEach(sp -> topDownProof(p, sp));
	}

	private void topDownProof(Program p, SuperPredicate sp) {
		System.out.println("Top-Down Proof of: " + sp.predicateName());
		HashSet<Rule> defRRules = sp.definedInRules();
		HashSet<Constant> objects = p.modelObjects();

	}
}
