package lang.relation;

import java.util.Set;
import java.util.TreeSet;

import lang.ast.Term;
import lang.relation.Binding.BindOverlap;
import lang.relation.Binding.BindResult;
import lang.relation.Binding.BindResult.Direction;
import lang.relation.Binding.BindResult.TaggedBind;
import lang.relation.Binding.BindTerm;

public class Relation {
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>();
	private int arity;
	public Binding binding = Binding.anyBinding;

	public Relation(Relation r) {
		this.arity = r.arity;
		this.relation = new TreeSet<PseudoTuple>();
		this.relation.addAll(r.relation);
	}

	public Relation(int arity) {
		this.arity = arity;
	}

	public static Relation of(PseudoTuple... tuples) {
		Relation r = new Relation(tuples.length);
		for (PseudoTuple t : tuples)
			r.relation.add(t);
		return r;
	}

	public Set<PseudoTuple> tuples() {
		return relation;
	}

	public int size() {
		return relation.size();
	}

	public int arity() {
		return arity;
	}

	public void addTuple(PseudoTuple pt) {
		relation.add(pt);
	}

	public void collectRelation(StringBuilder sb) {
		relation.forEach(pt -> pt.collectTuple(sb));
		sb.append("}");
	}

	/**
	 * Selects a set of tuples based on the binding disregarding the current names of the relation columns
	 */
	public Relation select(Binding selectBind) {
		Relation r = new Relation(this.arity);
		r.binding = selectBind;
		for(PseudoTuple t : relation) {
			if(selectBind.satisfiedBy(t)) {
				r.relation.add(t);
			}
		}
		return r;
	}
	
	public void addAll(Relation r) {
		relation.addAll(r.tuples());
	}
	
	/**
	 * Selects a set of tuples respecting the names of the relation columns
	 * Name is given by a Variable Name
	 * The semantics of a constant in the selectBind is that it is added to the 
	 * selected tuple at the given coordinate.
	 */
	public Relation selectNamed(Binding selectBind) {
		if(binding == Binding.anyBinding) return select(selectBind);
		Relation r = new Relation(selectBind.totalSize());
		r.binding = selectBind;
		TreeSet<BindOverlap> intersect = Binding.intersect(selectBind, binding);
		Binding difference = Binding.difference(intersect, selectBind);
		
		tuples().forEach(t -> {
			PseudoTuple tuple = new PseudoTuple(r.arity);
			for(BindOverlap bo : intersect) {
				Term val = t.coord(bo.b2.coords.first());
				for(Integer i : bo.b1.coords) {
					tuple.set(i, val);
				}
			}
			for(BindTerm bt : difference) {
				for(Integer i : bt.coords) {
					tuple.set(i, bt.t);
				}
			}
			r.addTuple(tuple);
		});
		return r;
	}

	private static boolean agrees(BindResult br, PseudoTuple t_left, PseudoTuple t_right, PseudoTuple build) {
		for (BindTerm bt_target : br.b_merged) {
			TaggedBind tagged = br.f.get(bt_target);
			if (tagged.d == Direction.Both) {
				BindOverlap bo = (BindOverlap) tagged.b;
				if (Term.termComparator.compare(t_left.coord(bo.b1.coords.first()),
						t_right.coord(bo.b2.coords.first())) != 0)
					return false;
				build.set(bt_target.coords.first(), t_left.coord(bo.b1.coords.first()));
			} else {
				PseudoTuple target = tagged.d == Direction.Left ? t_left : t_right;
				BindTerm bt = (BindTerm) tagged.b;
				build.set(bt_target.coords.first(), target.coord(bt.coords.first()));
			}
		}
		return true;
	}

	public static Relation join(Relation r1, Relation r2) {
		TreeSet<BindOverlap> intersect = Binding.intersect(r1.binding, r2.binding);
		BindResult br = Binding.merge(intersect, r1.binding, r2.binding);
		Relation r = new Relation(br.b_merged.size());
		r.binding = br.b_merged;

		r1.tuples().forEach(t_left -> {
			r2.tuples().forEach(t_right -> {
				PseudoTuple build = new PseudoTuple(r.arity);
				if (agrees(br, t_left, t_right, build))
					r.addTuple(build);
			});
		});
		return r;
	}
	
	public static Relation difference(Relation r1, Relation r2) {
		Relation r = new Relation(r1.arity);
		r1.tuples().forEach(t -> {
			if(!r2.contains(t)) r.addTuple(new PseudoTuple(t));
		});
		r.binding = r1.binding;
		return r;
	}

	public boolean contains(PseudoTuple t) {
		return relation.contains(t);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Relation))
			return false;
		return relation.equals(((Relation) obj).relation);
	}
}
