package lang.evaluation;

import java.util.Deque;
import java.util.Iterator;

import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.Rule;
import lang.ast.config.Description;
import lang.relation.Instantiation;
import lang.relation.PseudoTuple;
import lang.relation.Relation;
import lang.relation.Stratification;
import lang.relation.Stratification.Stratum;

public class BottomUpNaiveIterative extends InternalEvaluation {
	@Override
	public void evaluate(Program program, Description descr) {
		Deque<Stratum> order = Stratification.stratification(program);
		loadEBDFacts(program, descr);

		while (!order.isEmpty()) {
			evaluateStratum(order.pollFirst());
		}
	}

	private void immediateConsequenceHelper(Rule r, PseudoTuple t, Iterator<PseudoTuple> itr, Instantiation inst) {
//		for (RealLiteral rl : r.getHeadss()) {
//			System.out.println("Instantiate With: " + inst);
//			PseudoTuple tupHead = new PseudoTuple(rl.toTuple());
//			inst.instantiate(tupHead);
//
//			if (tupHead.isGround()) {
//				rl.getPredicate().formalpredicate().relation.addTuple(tupHead);
//			}
//		}
//
//		FormalPredicate fp = t.fp;
//		System.out.println("Current: " + fp + t);
//		Relation selected = fp.relation.select(inst.toBinding(t));
//
//		PseudoTuple next = null;
//		if (itr.hasNext()) {
//			next = itr.next();
//		}
//		
//		
//
//		for (PseudoTuple ps : selected.tuples()) {
//			System.out.println("Selected: " + ps);
//			Instantiation newInst = new Instantiation(t, ps);
//			
//			if(next != null) {
//				immediateConsequenceHelper(r, next, itr, Instantiation.combine(inst, newInst));
//			} else {
//				
//			}
//			// 
//		}
	}

	public void immediateConsequence(Rule r) {
		Iterator<PseudoTuple> itr = r.bodyTuples().iterator();
		if (!itr.hasNext())
			return;
		PseudoTuple t = itr.next();
		immediateConsequenceHelper(r, t, itr, new Instantiation());
	}

	public void evaluateStratum(Stratum strat) {

	}
}
