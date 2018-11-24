package lang.evaluation;

import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;

import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.Rule;
import lang.ast.config.Description;
import lang.relation.Binding;
import lang.relation.PseudoTuple;
import lang.relation.Relation;
import lang.relation.Stratification;
import lang.relation.Stratification.Stratum;

public class BottomUpNaiveIterative extends InternalEvaluation {
	@Override
	public void evaluate(Program program, Description descr) {
		Deque<Stratum> order = Stratification.stratificationForceCompute(program);
		loadEBDFacts(program, descr);
		
		while (!order.isEmpty()) {
			evaluateStratum(order.pollFirst());
		}
		dumpRelations(program, descr);
	}

	public boolean immediateConsequence(Rule r) {
		Iterator<RealLiteral> itr = r.getBodyList().iterator();
		
		Relation body_rel = null;
		while (itr.hasNext()) {
			RealLiteral rl_current = itr.next();
			PseudoTuple t_current = rl_current.toTuple();
			Binding b_current = Binding.createBinding(t_current);
			Relation r_current = rl_current.getPredicate().formalpredicate().relation.select(b_current);
			
			if (body_rel == null) {
				body_rel = r_current;
			} else {
				body_rel = Relation.join(body_rel, r_current);
			}
		}
		
		boolean changed = false;
		for (RealLiteral rl : r.getHeadss()) {
			Relation derived = body_rel.selectNamed(Binding.createBinding(rl.toTuple()));
			Relation prev = rl.getPredicate().formalpredicate().relation;
			int size = prev.size();
			prev.addAll(derived);
			if(prev.size() != size)
				changed = true;
		}
		return changed;
	}

	public void evaluateStratum(Stratum strat) {
		boolean changed = true;
		HashSet<Rule> rules = new HashSet<>();
		for(FormalPredicate fp : strat) {
			rules.addAll(fp.definedInRules());
		}
		while(changed) {
			changed = false;
			for(Rule r : rules) {
				boolean imm_change = immediateConsequence(r);
				changed = changed || imm_change;
			}
		}
	}
}
