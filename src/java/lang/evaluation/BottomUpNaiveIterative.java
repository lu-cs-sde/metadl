package lang.evaluation;

import java.util.Deque;
import java.util.HashSet;

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
		
		for(FormalPredicate fp : program.getFormalPredicates()) {
			fp.relation = new Relation(fp.realArity());
		}
		
		while (!order.isEmpty()) {
			evaluateStratum(program, descr, order.pollFirst());
		}
		dumpRelations(program, descr);
	}
	
	private Relation immediateConsequenceHelper(HashSet<Literal> literals, Relation body_rel) {
		for (Literal rl_current : literals) {
			PseudoTuple t_current = rl_current.toTuple();
			Binding b_current = Binding.createBinding(t_current);
			
			/**
			 * Select based on the literal selection rule, e.g. NEGLiteral will remove from the current body_rel.
			 */
			Relation r_current = rl_current.select(b_current, body_rel);

			if (body_rel == null) {
				body_rel = r_current;
			} else {
				body_rel = Relation.join(body_rel, r_current);
			}
		}
		return body_rel;
	}

	public boolean immediateConsequence(Program program, Description descr, Clause clause) {
		Relation body_rel = null;
		
		/**
		 * Find Body Relation if clause is a rule
		 */
		if (clause.isRule()) {
			Rule r = (Rule) clause;
			body_rel = immediateConsequenceHelper (r.positiveBodyLiterals(), body_rel);
			body_rel = immediateConsequenceHelper (r.negativeBodyLiterals(), body_rel);
		}

		boolean changed = false;
		for (FormalLiteral fl : clause.getHeadss()) {
			Relation derived;
			if(clause.isRule()) {
				derived = body_rel.selectNamed(Binding.createBinding(fl.toTuple()));
			} else {
				derived = new Relation(fl.arity());
				derived.addTuple(fl.toTuple());
			}
			
			Relation prev = fl.predicate().formalpredicate().relation;
			int size = prev.size();
			prev.addAll(derived);
			
			if (prev.size() != size) {
				changed = true;
				
				/**
				 * Process Potential Side-Effects Such as EDB-loading.
				 */
				fl.sideEffect(program, descr);
			}
		}
		return changed;
	}

	public void evaluateStratum(Program p, Description d, Stratum strat) {
		boolean changed = true;
		HashSet<Clause> rules = new HashSet<>();
		for (FormalPredicate fp : strat) {
			rules.addAll(fp.definedIn());
		}
		
		while (changed) {
			changed = false;
			for (Clause c : rules) {
				boolean imm_change = immediateConsequence(p,d,c);
				changed = changed || imm_change;
			}
		}
	}
}
