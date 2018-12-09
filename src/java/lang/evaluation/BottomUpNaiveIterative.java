package lang.evaluation;

import java.util.Deque;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.Clause;
import lang.ast.Fact;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.InclusiveLiteral;
import lang.ast.Literal;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.Rule;
import lang.ast.config.Description;
import lang.io.SimpleLogger;
import lang.relation.Binding;
import lang.relation.PseudoTuple;
import lang.relation.Relation;
import lang.relation.Stratification;
import lang.relation.Stratification.Stratum;

public class BottomUpNaiveIterative extends InternalEvaluation {
	
	@Override
	public void evaluate(Program program, Description descr) {
		//long start = System.nanoTime();
		FormalPredicate output = program.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);
		if(output == null) {
			SimpleLogger.logger().log("Nothing to output ... ", SimpleLogger.LogLevel.Level.DEBUG);
			return;
		}
		
		Deque<Stratum> order = Stratification.stratificationForceCompute(program);
		for(FormalPredicate fp : program.getFormalPredicates()) {
			fp.literal().initialSideEffect(program, descr);
		}
		
		TreeSet<FormalPredicate> computedPredicates = new TreeSet<FormalPredicate>(FormalPredicate.formalPredicateComparator);
		boolean isDone = false;
		while (!order.isEmpty() && !isDone) {
			Stratum nextStrat = order.pollFirst();
			evaluateStratum(program, descr, nextStrat);
			nextStrat.forEach(fp -> computedPredicates.add(fp));
			
			if(!computedPredicates.contains(output)) continue;
			isDone = true;
			for(PseudoTuple pt : output.relation.tuples()) {
				if(!computedPredicates.contains(((PredicateRef)pt.coord(0)).formalpredicate())) {
					isDone = false;
					break;
				}
			}
		}
		
		//long end = System.nanoTime();
		//double elapsed = (end - start) / 1000000;
//		System.out.print(elapsed);
		dumpRelations(program, descr);
	}
	
	private Relation immediateConsequenceHelper(Set<Literal> literals, Relation body_rel) {
		for (Literal rl_current : literals) {
			
			/**
			 * Select based on the literal selection rule, e.g. NEGLiteral will remove from the current body_rel.
			 */
			body_rel = rl_current.accumBody(body_rel);
		}
		return body_rel;
	}

	public boolean immediateConsequence(Program program, Description descr, Rule r) {
		Relation body_rel = Relation.nullRelation;
		
		/**
		 * Find Body Relation if clause is a rule
		 */
		body_rel = immediateConsequenceHelper (r.inclusiveBodyLiterals(), body_rel);
		body_rel = immediateConsequenceHelper (r.exclusiveBodyLiterals(), body_rel);

		boolean changed = false;
		for (InclusiveLiteral il : r.getHeadss()) {
			Relation derived = body_rel.selectNamed(Binding.createBinding(il.toTuple()));
			Relation prev = il.predicate().formalpredicate().relation;
			Relation delta = Relation.difference(derived, prev);
			prev.addAll(delta);
			
			if (delta.size() != 0) {
				changed = true;
				
				/**
				 * Process Potential Side-Effects Such as EDB-loading.
				 */
				il.sideEffect(program, descr, delta);
			}
		}
		return changed;
	}

	public void evaluateStratum(Program p, Description d, Stratum strat) {
		boolean changed = true;
		HashSet<Rule> rules = new HashSet<Rule>();
		
		/**
		 * Collect rules for stratum and load facts.
		 */
		for (FormalPredicate fp : strat) {
			for(Clause c : fp.definedIn()) {
				if(c.isRule()) {
					rules.add((Rule)c);
				} else {
					Fact f = (Fact) c;
					for(InclusiveLiteral il : f.getHeadss()) {
						Relation derived = new Relation(il.arity());
						derived.addTuple(il.toTuple());
						Relation delta = Relation.difference(derived, il.predicate().formalpredicate().relation);
						il.predicate().formalpredicate().relation.addAll(delta);
						
						/**
						 * Process Potential Side-Effects Such as EDB-loading.
						 */
						il.sideEffect(p, d, delta);
					}
				}
			}
		}
		
		while (changed) {
			changed = false;
			for (Rule r : rules) {
				boolean imm_change = immediateConsequence(p,d,r);
				changed = changed || imm_change;
			}
		}
	}
}
