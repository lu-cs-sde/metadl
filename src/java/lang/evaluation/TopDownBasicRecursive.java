package lang.evaluation;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import lang.ast.Constant;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.Rule;
import lang.ast.Term;
import lang.ast.Variable;
import lang.ast.config.Description;
import lang.io.CSVUtil;
import lang.io.SimpleLogger;
import lang.relation.ListUtil;
import lang.relation.PseudoTuple;

public class TopDownBasicRecursive extends InternalEvaluation {
	@Override
	public void evaluate(Program program, Description descr) {
		if (!descr.isTopDownBasic()) {
			SimpleLogger.logger().log("Can not perform TopDownBasicRecursive on non-topdownbasic-program",
					SimpleLogger.LogLevel.Level.ERROR);
			System.exit(0);
		}
		loadEBDFacts(program, descr);
		program.getFormalPredicates().forEach(fp -> {
			deriveAllFacts(fp, program);
			CSVUtil.dumpFileInto(fp, new File(descr.outputDir() + "/" + fp.predicateName() + ".csv"));
		});
	}

	public Set<PseudoTuple> allObjectTuples(Program p, int size, FormalPredicate sp) {
		HashSet<PseudoTuple> inst = new HashSet<PseudoTuple>();
		HashMap<Integer, Constant> constMap = new HashMap<>();

		int i = 0;
		for (Constant c : p.objects)
			constMap.put(i++, c);

		int N = (int) Math.pow(p.objects.size(), size);
		for (i = 0; i != N; ++i) {
			java.util.List<Integer> rad = ListUtil.toRadix(i, p.objects.size(), size);
			PseudoTuple tup = new PseudoTuple(sp, size);

			for (int j = 0; j != rad.size(); ++j) {
				tup.instantiate(j, constMap.get(rad.get(j)));
			}
			inst.add(tup);
		}
		return inst;
	}

	public HashSet<TreeMap<Variable, Constant>> allInstantiations(Program p, TreeSet<Variable> freeVars) {
		HashSet<TreeMap<Variable, Constant>> inst_set = new HashSet<TreeMap<Variable, Constant>>();
		if (freeVars.size() > p.objects.size())
			return inst_set;
		Set<PseudoTuple> universe = allObjectTuples(p, freeVars.size(), null);
		PseudoTuple var_tuple = new PseudoTuple(freeVars);
		universe.forEach(ground -> inst_set.add(var_tuple.createInstantiationFrom(ground)));
		return inst_set;
	}

	public TreeSet<Variable> freeVariables(HashSet<PseudoTuple> tuples) {
		TreeSet<Variable> freeVars = new TreeSet<Variable>(Term.termComparator);
		tuples.forEach(t -> freeVars.addAll(t.freeVariables()));
		return freeVars;
	}

	/**
	 * Takes a set of Propositions which must ALL hold in some given proof.
	 */
	private boolean proofAND(HashSet<PseudoTuple> propositions, Program p, TreeSet<PseudoTuple> visited) {
		for (PseudoTuple prop : propositions) {
			if (!topDownProof(prop, p, visited)) {
				return false;
			} else {
				prop.fp.relation.addTuple(prop);
			}
		}
		return true;
	}

	private boolean proofOR(PseudoTuple t, Program p, TreeSet<PseudoTuple> visited) {
		boolean haveProof = false;
		for (Rule rule : t.fp.definedInRules()) {
			RealLiteral head = t.fp.findRuleHead(rule);

			/**
			 * Bind the free variables in the head. Need to disqualify certain rules if the
			 * head cannot be bound correctly. E.g. if try to prove A(o1, o2) and have rule
			 * A(o3, y) :- ... with o1 != o3.
			 */
			PseudoTuple t_head = new PseudoTuple(head);
			if (t_head.instantiableAs(t)) {
				TreeMap<Variable, Constant> inst = t_head.createInstantiationFrom(t);
				HashSet<PseudoTuple> bodyTuples = new HashSet<PseudoTuple>();
				rule.bodyTuples().forEach(bt -> bodyTuples.add(new PseudoTuple(bt)));

				bodyTuples.forEach(ps -> ps.instantiateWith(inst));

				TreeSet<Variable> freeVars = freeVariables(bodyTuples);

				/**
				 * Will do lots of unnecessary work here since instantiations not calculated
				 * lazily! This is only a proof of concept, thus keep as is for now for clarity.
				 */
				HashSet<TreeMap<Variable, Constant>> instantiations = allInstantiations(p, freeVars);

				for (TreeMap<Variable, Constant> instMap : instantiations) {
					HashSet<PseudoTuple> bodyTuples2 = new HashSet<>();
					bodyTuples.forEach(tup -> {
						PseudoTuple cpy = new PseudoTuple(tup);
						cpy.instantiateWith(instMap);
						bodyTuples2.add(cpy);
					});

					if (proofAND(bodyTuples2, p, visited)) {
						t.fp.relation.addTuple(t);
						return true;
					}
				}
			}
		}
		return haveProof;
	}

	/**
	 * Takes a GROUND tuple t, and attempts to prove that it is included in the
	 * relation R in the context of program P.
	 */
	private boolean topDownProof(PseudoTuple t, Program p, TreeSet<PseudoTuple> visited) {
		if (visited.contains(t)) {
			return t.fp.relation.contains(t);
		}
		visited.add(t);

		if (!t.isGround()) {
			SimpleLogger.logger().log("Can only prove ground facts", SimpleLogger.LogLevel.Level.ERROR);
			return false;
		}
		if (!p.mayProve(t)) {
			SimpleLogger.logger().log("The provided tuple cannot be proven within the program.",
					SimpleLogger.LogLevel.Level.ERROR);
			return false;
		}
		if (t.size != t.fp.realArity()) {
			SimpleLogger.logger().log("The tuple to be proven has the wrong arity", SimpleLogger.LogLevel.Level.ERROR);
			return false;
		}
		if (t.fp.relation.contains(t))
			return true;
		return proofOR(t, p, visited);
	}

	/**
	 * Attempts to prove fact t in relation t.sp in context of program p
	 */
	public boolean deriveFact(PseudoTuple t, Program p) {
		return topDownProof(t, p, new TreeSet<PseudoTuple>());
	}

	/**
	 * Derives all facts corresponding to the relation sp in the context of program
	 * p.
	 */
	public void deriveAllFacts(FormalPredicate sp, Program p) {
		Set<PseudoTuple> universe = allObjectTuples(p, sp.realArity(), sp);
		universe.forEach(t -> {
			deriveFact(t, p);
		});
	}
}
