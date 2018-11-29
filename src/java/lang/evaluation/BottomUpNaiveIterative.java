package lang.evaluation;

import java.util.Deque;
import java.util.HashSet;

import lang.ast.Clause;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.InclusiveLiteral;
import lang.ast.Literal;
import lang.ast.Program;
import lang.ast.Rule;
import lang.ast.config.Description;
import lang.relation.Binding;
import lang.relation.Relation;
import lang.relation.Stratification;
import lang.relation.Stratification.Stratum;

public class BottomUpNaiveIterative extends InternalEvaluation {
	@Override
	public void evaluate(Program program, Description descr) {
		Deque<Stratum> order = Stratification.stratificationForceCompute(program);
		
		System.out.println("FP: " + program.predicateSymbols());
		for(FormalPredicate fp : program.getFormalPredicates()) {
			System.out.println("FP: " + fp);
			fp.literal().initialSideEffect(program, descr);
		}
		while (!order.isEmpty()) {
			evaluateStratum(program, descr, order.pollFirst());
		}
		
		dumpRelations(program, descr);
	}
	
	private Relation immediateConsequenceHelper(HashSet<Literal> literals, Relation body_rel) {
		for (Literal rl_current : literals) {
			
			/**
			 * Select based on the literal selection rule, e.g. NEGLiteral will remove from the current body_rel.
			 */
			Relation r_current = rl_current.select(body_rel);

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
			body_rel = immediateConsequenceHelper (r.inclusiveBodyLiterals(), body_rel);
			body_rel = immediateConsequenceHelper (r.exclusiveBodyLiterals(), body_rel);
		}

		boolean changed = false;
		for (InclusiveLiteral il : clause.getHeadss()) {
			Relation derived;
			if(clause.isRule()) {
				derived = body_rel.selectNamed(Binding.createBinding(il.toTuple()));
			} else {
				derived = new Relation(il.arity());
				derived.addTuple(il.toTuple());
			}
			
			Relation prev = il.predicate().formalpredicate().relation;
			int size = prev.size();
			prev.addAll(derived);
			
			if (prev.size() != size) {
				changed = true;
				
				/**
				 * Process Potential Side-Effects Such as EDB-loading.
				 */
				il.sideEffect(program, descr);
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
