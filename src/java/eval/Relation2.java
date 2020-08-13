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
	private TreeSet<Tuple> currentSet;

	public Relation2(int arity, String name) {
		this(arity);
		this.name = name;
	}

	@Override public String toString() {
		return getName() + "(" + arity() + ")";
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
		currentSet = new TreeSet<>(defaultIndex);
		indexedMaps.put(defaultIndex, currentSet);
	}

	public void setIndex(Index index) {
		TreeSet<Tuple> nextSet = indexedMaps.get(index);
		if (nextSet == null) {
			// no set for the requested index, add new one
			nextSet = new TreeSet<>(index);
			indexedMaps.put(index, nextSet);

			// initialize the set with all the values in the current
			// set
			nextSet.addAll(currentSet);
		}
		currentSet = nextSet;
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

	public SortedSet<Tuple> lookup(Tuple loInclusive, Tuple hiInclusive) {
		assert loInclusive.arity() == hiInclusive.arity();
		return Collections.unmodifiableSortedSet(currentSet.subSet(loInclusive, true, hiInclusive, true));
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
		return currentSet.size();
	}

	public SortedSet<Tuple> tuples() {
		return Collections.unmodifiableSortedSet(currentSet);
	}

	public void clear() {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.clear();
	}
}
