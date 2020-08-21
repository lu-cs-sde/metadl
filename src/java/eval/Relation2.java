package eval;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public class Relation2 {
	private int arity = 0;
	private String name = "AnonymousRelation";

	private Map<Index, TreeSet<Tuple>> indexedMaps = new HashMap<>();
	private TreeSet<Tuple> defaultSet;

	public Relation2(int arity, String name) {
		this(arity);
		this.name = name;
	}

	@Override public String toString() {
		return name + "(" + arity + ")" + tuples().toString();
	}

	public String getName() {
		return name;
	}

	public Relation2(int arity) {
		this.arity = arity;

		// create a default index consisting of all columns
		List<Integer> defaultIndices = new ArrayList<>();
		for (int i = 0; i < arity; ++i)
			defaultIndices.add(i);

		Index defaultIndex = new Index(defaultIndices, arity);
		defaultSet = new TreeSet<>(defaultIndex);
		indexedMaps.put(defaultIndex, defaultSet);
	}

	private synchronized TreeSet<Tuple> setIndex(Index index) {
		TreeSet<Tuple> nextSet = indexedMaps.get(index);
		if (nextSet == null) {
			// no set for the requested index, add new one
			nextSet = new TreeSet<>(index);
			indexedMaps.put(index, nextSet);

			// initialize the set with all the values in the current
			// set
			nextSet.addAll(defaultSet);
		}
		return nextSet;
	}

	public Tuple infTuple() {
		Tuple t = new Tuple(arity);
		for (int i = 0; i < arity; ++i)
			t.set(i, Long.MIN_VALUE);
		return t;
	}

	public Tuple supTuple() {
		Tuple t = new Tuple(arity);
		for (int i = 0; i < arity; ++i)
			t.set(i, Long.MAX_VALUE);
		return t;
	}

	public SortedSet<Tuple> lookup(Index index, Tuple loInclusive, Tuple hiInclusive) {
		TreeSet<Tuple> currentSet = setIndex(index);
		assert loInclusive.arity() == hiInclusive.arity();
		return Collections.unmodifiableSortedSet(currentSet.subSet(loInclusive, true, hiInclusive, true));
	}

	public boolean hasEntryInRange(Index index, Tuple loInclusive, Tuple hiInclusive) {
		TreeSet<Tuple> currentSet = setIndex(index);
		assert loInclusive.arity() == hiInclusive.arity();
		// find min e s.t. e >= loInclusive
		Tuple e = currentSet.ceiling(loInclusive);
		if (e == null)
			return false;
		// return e <= hiInclusive
		return currentSet.comparator().compare(e, hiInclusive) <= 0;
	}

	public boolean insert(Tuple t) {
		boolean change = false;
		for (SortedSet<Tuple> s : indexedMaps.values())
			change |= s.add(t);
		return change;
	}

	public boolean insert(Collection<? extends Tuple> ts) {
		boolean change = false;
		for (SortedSet<Tuple> s : indexedMaps.values())
			change |= s.addAll(ts);
		return change;
	}

	public int arity() {
		return arity;
	}

	public int size() {
		return defaultSet.size();
	}

	public SortedSet<Tuple> tuples() {
		return Collections.unmodifiableSortedSet(defaultSet);
	}

	public void clear() {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.clear();
	}

	@Override public boolean equals(Object o) {
		if (!(o instanceof Relation2))
			return false;
		Relation2 other = (Relation2) o;
		return this.arity == other.arity &&
			this.name.equals(other.name) &&
			this.tuples().equals(other.tuples());
	}

	@Override public int hashCode() {
		int p = 31;
		return ((name.hashCode() * p) + tuples().hashCode()) * p  + arity;
	}
}
