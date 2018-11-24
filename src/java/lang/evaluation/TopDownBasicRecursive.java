package lang.evaluation;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
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
import lang.relation.Instantiation;
import lang.relation.ListUtil;
import lang.relation.PseudoTuple;

public class TopDownBasicRecursive extends InternalEvaluation {
	
	public class TaggedTuple extends PseudoTuple {
		private FormalPredicate fp;
		
		public TaggedTuple(TaggedTuple t) {
			super(t);
			this.fp = t.fp;
		}
		
		public TaggedTuple(FormalPredicate fp) {
			super(fp.realArity());
			this.fp = fp;
		}
		public TaggedTuple(FormalPredicate fp, PseudoTuple t) {
			super(t);
			this.fp = fp;
		}
		public TaggedTuple(FormalPredicate fp, TreeSet<Variable> vars) {
			super(vars);
			this.fp = fp;
		}
		
		public TaggedTuple(RealLiteral rl) {
			super(rl);
			this.fp = rl.getPredicate().formalpredicate();
		}
		
		@Override
		public int compareTo(PseudoTuple arg0) {
			if(arg0 instanceof TaggedTuple) {
				int pred_comp = fp.predicateName().compareTo(((TaggedTuple)arg0).fp.predicateName());
				if(pred_comp != 0) return pred_comp;
			}
			return super.compareTo(arg0);
		}
	}
	
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

	public Set<PseudoTuple> allObjectTuples(Program p, int size) {
		HashSet<PseudoTuple> inst = new HashSet<PseudoTuple>();
		HashMap<Integer, Constant> constMap = new HashMap<>();

		int i = 0;
		for (Constant c : p.objects)
			constMap.put(i++, c);

		int N = (int) Math.pow(p.objects.size(), size);
		for (i = 0; i != N; ++i) {
			java.util.List<Integer> rad = ListUtil.toRadix(i, p.objects.size(), size);
			PseudoTuple tup = new PseudoTuple(size);

			for (int j = 0; j != rad.size(); ++j) {
				tup.instantiate(j, constMap.get(rad.get(j)));
			}
			inst.add(tup);
		}
		return inst;
	}

	public HashSet<Instantiation> allInstantiations(Program p, TreeSet<Variable> freeVars) {
		HashSet<Instantiation> inst_set = new HashSet<Instantiation>();
		if (freeVars.size() > p.objects.size())
			return inst_set;
		Set<PseudoTuple> universe = allObjectTuples(p, freeVars.size());
		PseudoTuple var_tuple = new PseudoTuple(freeVars);
		universe.forEach(ground -> inst_set.add(new Instantiation(var_tuple, ground)));
		return inst_set;
	}

	public TreeSet<Variable> freeVariables(HashSet<TaggedTuple> tuples) {
		TreeSet<Variable> freeVars = new TreeSet<Variable>(Term.termComparator);
		tuples.forEach(t -> freeVars.addAll(t.freeVariables()));
		return freeVars;
	}

	/**
	 * Takes a set of Propositions which must ALL hold in some given proof.
	 */
	private boolean proofAND(HashSet<TaggedTuple> propositions, Program p, TreeSet<TaggedTuple> visited) {
		for (TaggedTuple prop : propositions) {
			if (!topDownProof(prop, p, visited)) {
				return false;
			} else {
				prop.fp.relation.addTuple(prop);
			}
		}
		return true;
	}

	private boolean proofOR(TaggedTuple t, Program p, TreeSet<TaggedTuple> visited) {
		boolean haveProof = false;
		for (Rule rule : t.fp.definedInRules()) {
			RealLiteral head = t.fp.findRuleHead(rule);

			/**
			 * Bind the free variables in the head. Need to disqualify certain rules if the
			 * head cannot be bound correctly. E.g. if try to prove A(o1, o2) and have rule
			 * A(o3, y) :- ... with o1 != o3.
			 */
			TaggedTuple t_head = new TaggedTuple(head);
			if (Instantiation.instantiableAs(t_head, t)) {
				Instantiation inst = new Instantiation(t_head, t);
				HashSet<TaggedTuple> bodyTuples = new HashSet<TaggedTuple>();
				
				rule.getBodyList().forEach(rl -> {
					bodyTuples.add(new TaggedTuple(rl));
				});
				bodyTuples.forEach(ps -> inst.instantiate(ps));
				TreeSet<Variable> freeVars = freeVariables(bodyTuples);

				/**
				 * Will do lots of unnecessary work here since instantiations not calculated
				 * lazily! This is only a proof of concept, thus keep as is for now for clarity.
				 */
				HashSet<Instantiation> instantiations = allInstantiations(p, freeVars);

				for (Instantiation instMap : instantiations) {
					HashSet<TaggedTuple> bodyTuples2 = new HashSet<>();
					bodyTuples.forEach(tup -> {
						TaggedTuple cpy = new TaggedTuple(tup);
						instMap.instantiate(cpy);
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
	private boolean topDownProof(TaggedTuple t, Program p, TreeSet<TaggedTuple> visited) {
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
	public boolean deriveFact(TaggedTuple t, Program p) {
		return topDownProof(t, p, new TreeSet<TaggedTuple>());
	}

	/**
	 * Derives all facts corresponding to the relation sp in the context of program
	 * p.
	 */
	public void deriveAllFacts(FormalPredicate fp, Program p) {
		Set<PseudoTuple> universe = allObjectTuples(p, fp.realArity());
		universe.forEach(t -> {
			deriveFact(new TaggedTuple(fp, t), p);
		});
	}
}
