package eval;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

public class Relation2 {
	int arity = 0;
	String name = "AnonRel";

	private Map<Index, TreeSet<Tuple>> indexedMaps = new HashMap<>();
	private TreeSet<Tuple> currentSet = null;
	private Index defaultIndex;

	public Relation2(int arity, String name) {
		this(arity);
		this.name = name;
	}

	@Override public String toString() {
		return "name(" + arity + ")";
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

		defaultIndex = new Index(defaultIndices, arity);
		setIndex(defaultIndex);
	}

	public void setIndex(Index index) {
		currentSet = indexedMaps.get(index);
		if (currentSet == null) {
			// no set for the requested index, add new one
			currentSet = new TreeSet<>(index);
			indexedMaps.put(index, currentSet);

			// initialize the current set with all the values
			for (SortedSet<Tuple> s : indexedMaps.values()) {
				currentSet.addAll(s);
				break;
			}
		}
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
		return currentSet.subSet(loInclusive, true, hiInclusive, true);
	}

	public void insert(Tuple t) {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.add(t);
	}

	public void insert(Collection<? extends Tuple> ts) {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.addAll(ts);
	}

	public int arity() {
		return arity;
	}

	public int size() {
		return indexedMaps.get(defaultIndex).size();
	}
}
