package eval;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;


class Index implements Comparator<Tuple> {
	private List<Integer> indices;
	private List<Integer> otherIndices;

	public Index(Collection<Integer> indices, int arity) {
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
		if (!(o instanceof Index))
			return false;
		Index other = (Index) o;
		return indices.equals(other.indices);
	}

	@Override
	public int hashCode() {
		return indices.hashCode();
	}
}
