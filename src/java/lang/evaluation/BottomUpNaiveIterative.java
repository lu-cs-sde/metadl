package lang.evaluation;

import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;

import lang.ast.Clause;
import lang.ast.FormalLiteral;
import lang.ast.FormalPredicate;
import lang.ast.Literal;
import lang.ast.Program;
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
		System.out.println(order);
		while (!order.isEmpty()) {
			evaluateStratum(order.pollFirst());
		}
		dumpRelations(program, descr);
	}

	public boolean immediateConsequence(Clause c) {
		Relation body_rel = null;
		if (c.isRule()) {
			Iterator<Literal> itr = ((Rule) c).getBodyList().iterator();
			while (itr.hasNext()) {
				Literal rl_current = itr.next();

				PseudoTuple t_current = rl_current.toTuple();
				Binding b_current = Binding.createBinding(t_current);
				Relation r_current = rl_current.predicate().formalpredicate().relation.select(b_current);

				if (body_rel == null) {
					body_rel = r_current;
				} else {
					body_rel = Relation.join(body_rel, r_current);
				}
			}
		}

		boolean changed = false;
		for (FormalLiteral rl : c.getHeadss()) {
			Relation derived;
			if(c.isRule()) {
				derived = body_rel.selectNamed(Binding.createBinding(rl.toTuple()));
			} else {
				derived = new Relation(rl.arity());
				derived.addTuple(rl.toTuple());
			}
			
			Relation prev = rl.predicate().formalpredicate().relation;
			int size = prev.size();
			prev.addAll(derived);
			if (prev.size() != size) {
				changed = true;
				if (rl.isEDB()) {
					/**
					 * Load new tuples
					 */
				}
			}
		}
		return changed;
	}

	public void evaluateStratum(Stratum strat) {
		boolean changed = true;
		HashSet<Clause> rules = new HashSet<>();
		for (FormalPredicate fp : strat) {
			rules.addAll(fp.definedIn());
			fp.relation = new Relation(fp.realArity());
		}
		while (changed) {
			changed = false;
			for (Clause c : rules) {
				boolean imm_change = immediateConsequence(c);
				changed = changed || imm_change;
			}
		}
	}
}
