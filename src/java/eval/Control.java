package eval;

import java.util.List;
import java.util.SortedSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

public interface Control {
	void eval();

	public static Control forAll(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test,
								 List<Pair<Integer, Long>> consts,
								 List<Pair<Integer, Integer>> assign,
								 Control cont) {
		return new ForAll(t, rel, test, consts, assign, cont);
	}

	public static Control ifNotExists(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test,
									  List<Pair<Integer, Long>> consts,
									  Control cont) {
		return new IfNotExists(t, rel, test, consts, cont);
	}

	public static Control insert(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> assign,
								 List<Pair<Integer, Long>> consts, Control cont) {
		return new Insert(t, rel, assign, consts, cont);
	}

	public static Control nop() {
		return new Control() {
			@Override public void eval() {
				// do nothing
			}
		};
	}

	public static Control eq(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a == b;
			}
		};
	}

	public static Control neq(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a != b;
			}
		};
	}

	public static Control gt(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a > b;
			}
		};
	}

	public static Control lt(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a < b;
			}
		};
	}

	public static Control gte(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a >= b;
			}
		};
	}

	public static Control lte(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a <= b;
			}
		};
	}

	public static Control bind(Tuple t, Control cont, int dst, Operation r) {
		return new Control() {
			@Override public void eval() {
				t.set(dst, r.eval());
				cont.eval();
			}

		};
	}

	// TODO: implement match
}

class ForAll implements Control {
	private Tuple t;
	private Relation2 rel;
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Integer>> assign;
	private List<Pair<Integer, Long>> consts;

	private Control cont;
	private long prefix[];
	private Index index;
	/**
	   test - pairs of (columns, tuple position) to test for equality
	   assign - pairs of (columns, tuple position) to assign
	 */
	ForAll(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test, List<Pair<Integer, Long>> consts,
		   List<Pair<Integer, Integer>> assign, Control cont) {
		this.t = t;
		this.rel = rel;
		this.cont = cont;
		this.consts = consts;
		index = new Index(Stream.concat(test.stream(), consts.stream())
						  .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());

		// allocate memory for the prefix
		prefix = new long[test.size() + consts.size()];
		// populate the constant part
		for (Pair<Integer, Long> c : consts) {
			prefix[c.getLeft()] = c.getRight();
		}
	}

	public void eval() {
		// set an index for the relation
		rel.setIndex(index);

		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			prefix[c.getLeft()] = t.get(c.getRight());
		}

		SortedSet<Tuple> tuples = rel.lookup(prefix);
		for (Tuple r : tuples) {
			for (Pair<Integer, Integer> p : assign) {
				t.set(p.getRight(), r.get(p.getLeft()));
			}
			cont.eval();
		}
	}
}

class IfNotExists implements Control {
	private Tuple t;
	private Relation2 rel;
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Long>> consts;
	private Control cont;
	private long prefix[];
	private Index index;


	IfNotExists(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test,
				List<Pair<Integer, Long>> consts, Control cont) {
		this.t = t;
		this.rel = rel;
		this.cont = cont;
		this.consts = consts;
		index = new Index(Stream.concat(test.stream(), consts.stream())
						  .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());
		// allocate memory for prefix
		prefix = new long[test.size() + consts.size()];
		// populate the constant part
		for (Pair<Integer, Long> c : consts) {
			prefix[c.getLeft()] = c.getRight();
		}
	}

	public void eval() {
		rel.setIndex(index);
		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			prefix[c.getLeft()] = t.get(c.getRight());
		}

		SortedSet<Tuple> tuples = rel.lookup(prefix);
		if (tuples.isEmpty())
			cont.eval();
	}
}

class Insert implements Control {
	private Tuple t;
	private List<Pair<Integer, Integer>> assign;
	private List<Pair<Integer, Long>> consts;
	private Control cont;
	private Relation2 rel;

	/**
	   t - tuple to draw the data from
	   rel - relation to insert into
	   assign - list of (column, tuple position) to copy data from tuple entry to colum
	   consts - list of (column, const) to insert const into column
	   cont - continuation
	 */
	Insert(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> assign, List<Pair<Integer, Long>> consts, Control cont) {
		this.t = t;
		this.rel = rel;
		this.assign = assign;
		this.consts = consts;
		this.cont = cont;
	}

	@Override
	public void eval() {
		Tuple u = new Tuple(rel.arity());
		for (Pair<Integer, Integer> a : assign) {
			u.set(a.getLeft(), t.get(a.getRight()));
		}

		for (Pair<Integer, Long> c : consts) {
			u.set(c.getLeft(), c.getRight());
		}

		cont.eval();
	}
}

abstract class Test implements Control {
	private Control cont;
	private Operation left, right;

	public Test(Control cont, Operation left, Operation right) {
		this.cont = cont;
		this.left = left;
		this.right = right;
	}

	public void eval() {
		if (test(left.eval(), right.eval()))
			cont.eval();
	}

	protected abstract boolean test(long a, long b);
}
