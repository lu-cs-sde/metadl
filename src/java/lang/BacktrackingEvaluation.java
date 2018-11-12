package lang;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

import lang.ast.Constant;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.Rule;
import lang.ast.SuperPredicate;
import lang.ast.Term;
import lang.ast.Variable;
import lang.relation.PseudoTuple;
import lang.relation.ListUtil;

public class BacktrackingEvaluation extends InternalEvaluation {
	private static ListUtil<Integer> setUtil = new ListUtil<Integer>();

	@Override
	void evaluate(Program p) {
		System.out.println("Evaluate Backtracking");
		loadEBDFacts(p);
		// p.getSuperPredicates().forEach(sp -> topDownProof(p, sp));
	}
	
	public Set<PseudoTuple> allObjectTuples(Program p, int size) {
		HashSet<PseudoTuple> inst = new HashSet<PseudoTuple>();
		HashMap<Integer, Constant> constMap = new HashMap<>();
		
		int i = 0;
		for(Constant c : p.objects)
			constMap.put(i++, c);
		
		int N = (int)Math.pow(p.objects.size(), size);
		for(i = 0; i != N; ++i) {
			java.util.List<Integer> rad = ListUtil.toRadix(i, p.objects.size(), size);
			PseudoTuple tup = new PseudoTuple(size);
		
			for(int j = 0; j != rad.size(); ++j) {
				tup.instantiate(j, constMap.get(rad.get(j)));
			}
			inst.add(tup);
		}
		return inst;
	}

	private HashSet<TreeMap<Variable, Constant>> allInstantiations(Program p, TreeSet<Variable> freeVars) {
		HashSet<TreeMap<Variable, Constant>> inst_set = new HashSet<TreeMap<Variable, Constant>>();
		if (freeVars.size() > p.objects.size())
			return inst_set;
		Set<PseudoTuple> universe = allObjectTuples(p, freeVars.size());
		PseudoTuple var_tuple = new PseudoTuple(freeVars);
		universe.forEach(ground -> inst_set.add(var_tuple.createInstantiationFrom(ground)));
		return inst_set;
	}

	private TreeSet<Variable> freeVariables(HashSet<PseudoTuple> tuples) {
		TreeSet<Variable> freeVars = new TreeSet<Variable>(Term.termComparator);
		tuples.forEach(t -> freeVars.addAll(t.freeVariables()));
		return freeVars;
	}

	/**
	 * Takes a set of Propositions which must ALL hold in some given proof.
	 */
	private boolean proofAND(HashSet<PseudoTuple> propositions, Program p) {
		/**
		 * PseudoTuple needs the corresponding SuperPredicate Here.
		 * 
		 */
		
		return true;
	}

	private boolean proofOR(PseudoTuple t, SuperPredicate r, Program p) {
		boolean haveProof = false;

		for (Rule rule : r.definedInRules()) {
			/**
			 * Instantiate Body with the tuple we are trying to show.
			 */
			RealLiteral head = r.findRuleHead(rule);
			assertTrue(head != null);

			/**
			 * Bind the free variables in the head. Need to disqualify certain rules if the
			 * head cannot be bound correctly. E.g. if try to prove A(o1, o2) and have rule
			 * A(o3, y) :- ... with o1 != o3.
			 */
			PseudoTuple t_head = new PseudoTuple(head);
			if (t_head.instantiableAs(t)) {
				TreeMap<Variable, Constant> inst = t_head.createInstantiationFrom(t);
				HashSet<PseudoTuple> bodyTuples = rule.bodyTuples();
				bodyTuples.forEach(ps -> ps.instantiateWith(inst));

				TreeSet<Variable> freeVars = freeVariables(bodyTuples);

				/**
				 * Will do lots of unnecessary work here since instantiations not calculated
				 * lazily! This is only a proof of concept, thus keep as is for now.
				 */
				HashSet<TreeMap<Variable, Constant>> instantiations = allInstantiations(p, freeVars);

				for (TreeMap<Variable, Constant> inst2 : instantiations) {
					HashSet<PseudoTuple> bodyTuples2 = new HashSet<>(bodyTuples);
					bodyTuples2.forEach(bt -> bt.instantiateWith(inst2));
					if (proofAND(bodyTuples2, p))
						return true;
				}
			}
		}
		return haveProof;
	}

	/**
	 * Takes a GROUND tuple t, and attempts to prove that it is included in the
	 * relation R in the context of program P.
	 */
	private boolean topDownProof(PseudoTuple t, SuperPredicate r, Program p) {
		System.out.println("Top-Down Proof of: " + r.predicateName());
		if (!t.isGround()) {
			System.err.println("Can only prove ground facts");
			return false;
		}
		if (!p.mayProve(t)) {
			System.out.println("The provided tuple cannot be proven within the program.");
			return false;
		}
		if (t.size != r.realArity()) {
			System.out.println("The tuple to be proven has the wrong arity");
			return false;
		}
		if (r.relation.contains(t))
			return true;
		return proofOR(t, r, p);
	}
}
