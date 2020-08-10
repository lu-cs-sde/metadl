package eval;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.tuple.Pair;


public interface Operation {
	void eval();
}

class ForAll implements Operation {
	private Tuple t;
	private Relation2 rel;
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Integer>> assign;
	long prefix[];

	Index index;
	/**
	   test - pairs of (columns, tuple position) to test for equality
	   assign - pairs of (columns, tuple position) to assign
	 */
	ForAll(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test, List<Pair<Integer, Integer>> assign, Operation cont) {
		this.t = t;
		this.rel = rel;
		index = new Index(test.stream().map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());
		prefix = new long[test.size()];
	}

	public void eval() {
		rel.setIndex(index);
	}
}
