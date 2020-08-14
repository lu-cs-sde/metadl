package lang.relation;

import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;
import java.util.Collection;

public class Relation {
	public static final Relation nullRelation = new Relation(0);
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>();
	private int arity;

	public Relation(Relation r) {
		this.arity = r.arity;
		this.relation = new TreeSet<PseudoTuple>();
		this.relation.addAll(r.relation);
	}

	public Relation(int arity) {
		this.arity = arity;
	}

	public static Relation of(PseudoTuple... tuples) {
		Relation r = new Relation(tuples[0].arity());
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

	public boolean addTuple(PseudoTuple pt) {
		return relation.add(pt);
	}

	public boolean addTuples(Collection<PseudoTuple> c) {
		return relation.addAll(c);
	}

	public void collectRelation(StringBuilder sb) {
		relation.forEach(pt -> pt.collectTuple(sb));
		sb.append("}");
	}

	public void expand() {
		tuples().forEach(t -> t.expand());
		arity = arity + 1;
	}

	public void addAll(Relation r) {
		relation.addAll(r.tuples());
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

	@Override
	public String toString() {
		return relation.toString();
	}

	/**
	   Concatenate two relations columnwise. If a relation contains more
	   tuples, just pad with default tuples.
	 */
	public static Relation cat(Relation left, PseudoTuple defaultLeft,
							   Relation right, PseudoTuple defaultRight) {
		Relation result = new Relation(right.arity() + left.arity());

		Iterator<PseudoTuple> itLeft = left.tuples().iterator();
		Iterator<PseudoTuple> itRight = right.tuples().iterator();

		while (itLeft.hasNext() && itRight.hasNext()) {
			PseudoTuple t = PseudoTuple.cat(itLeft.next(), itRight.next());
			result.addTuple(t);
		}

		while (itLeft.hasNext()) {
			PseudoTuple t = PseudoTuple.cat(itLeft.next(), defaultRight);
			result.addTuple(t);
		}

		while (itRight.hasNext()) {
			PseudoTuple t = PseudoTuple.cat(defaultLeft, itRight.next());
			result.addTuple(t);
		}

		return result;
	}
}
