package eval;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;


class IndexedComparator implements Comparator<Tuple> {
	private List<Integer> indices;
	private List<Integer> otherIndices;

	public IndexedComparator(Collection<Integer> indices, int arity) {
		assert arity >= indices.size();
		this.indices = new ArrayList<>(indices);
		Collections.sort(this.indices);
		this.otherIndices = new ArrayList<Integer>();
		for (int i = 0; i < arity; ++i) {
			if (!this.indices.contains(i))
				otherIndices.add(i);
		}
		assert this.indices.size() + otherIndices.size() == arity;
	}

	@Override
	public int compare(Tuple arg0, Tuple arg1) {
		for (int i : indices) {
			if (arg0.get(i) < arg1.get(i))
				return -1;
			if (arg0.get(i) > arg1.get(i))
				return 1;
		}

		for (int i : otherIndices) {
			if (arg0.get(i) < arg1.get(i))
				return -1;
			if (arg0.get(i) > arg1.get(i))
				return 1;
		}
		return 0;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof IndexedComparator))
			return false;
		IndexedComparator other = (IndexedComparator) o;
		return indices.equals(other.indices);
	}

	@Override
	public int hashCode() {
		return indices.hashCode();
	}
}

public class Relation2 {
	int arity = 0;

	Map<IndexedComparator, SortedSet<Tuple>> indexedMaps = new HashMap<>();
	SortedSet<Tuple> currentSet = null;

	public Relation2(int arity) {
		this.arity = arity;

		// create a default index consisting of all columns
		List<Integer> defaultIndices = new ArrayList<>();
		for (int i = 0; i < arity; ++i)
			defaultIndices.add(i);

		IndexedComparator defaultIndex = new IndexedComparator(defaultIndices, arity);
		setIndex(defaultIndex);
	}

	public void setIndex(IndexedComparator index) {
		currentSet = indexedMaps.get(index);
		if (currentSet == null) {
			currentSet = new TreeSet<>(index);
			indexedMaps.put(index, currentSet);
		}
	}

	public SortedSet<Tuple> lookup(long[] prefix) {
		assert prefix.length <= arity;
		assert currentSet != null;

		Tuple keyL = new Tuple(arity);
		Tuple keyH = new Tuple(arity);

		for (int i = 0; i < prefix.length; ++i) {
			keyL.set(i, prefix[i]);
			keyH.set(i, prefix[i]);
		}

		for (int i = prefix.length; i < arity; ++i) {
			keyL.set(i, Long.MIN_VALUE);
			keyH.set(i, Long.MAX_VALUE);
		}

		return currentSet.subSet(keyL, keyH);
	}

	public void insert(Tuple t) {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.add(t);
	}

	public void insert(Collection<? extends Tuple> ts) {
		for (SortedSet<Tuple> s : indexedMaps.values())
			s.addAll(ts);
	}

	private IndexedComparator indexFromPrefix(Set<Integer> prefix) {
		IndexedComparator index = new IndexedComparator(prefix, arity);
		return index;
	}
}
