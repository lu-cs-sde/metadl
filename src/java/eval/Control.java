package eval;

import java.util.List;
import java.util.SortedSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

class Util {
	static String indent(int n) {
		String s = "";
		for (int i = 0; i < n; ++i)
			s += "\t";
		return s;
	}
}

public interface Control {
	void eval();

	String prettyPrint(int indent);

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

			@Override public String prettyPrint(int indent) {
				return "";
			}
		};
	}

	public static Control eq(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a == b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s = %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}
		};
	}

	public static Control neq(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a != b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s != %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}
		};
	}

	public static Control gt(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a > b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s > %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}
		};
	}

	public static Control lt(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a < b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s < %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}

		};
	}

	public static Control gte(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a >= b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s >= %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}

		};
	}

	public static Control lte(Control cont, Operation l, Operation r) {
		return new Test(cont, l, r) {
			@Override public boolean test(long a, long b) {
				return a <= b;
			}
			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %s <= %s THEN\n", l.prettyPrint(), r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
			}

		};
	}

	public static Control match(EvaluationContext ctx, Control cont, Operation regex, Operation arg, boolean invert) {
		return new Test(cont, regex, arg) {
			@Override public boolean test(long a, long b) {
				String r = ctx.externalizeString(a);
				String s = ctx.externalizeString(b);
				return invert ^ Pattern.matches(r, s);
			}

			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("IF %smatch(%s, %s) THEN\n", invert ? "!" : "", regex.prettyPrint(), arg.prettyPrint());
			}
		};
	}

	public static Control bind(Tuple t, Control cont, int dst, Operation r) {
		return new Control() {
			@Override public void eval() {
				t.set(dst, r.eval());
				cont.eval();
			}

			@Override public String prettyPrint(int indent) {
				return Util.indent(indent) + String.format("t[%d] := %s\n", dst, r.prettyPrint())
					+ cont.prettyPrint(indent + 1);
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
	private Tuple minKey, maxKey;
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
		this.test = test;
		this.consts = consts;
		this.assign = assign;
		index = new Index(Stream.concat(test.stream(), consts.stream())
						  .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());

		// allocate memory for the prefix
		minKey = rel.infTuple();
		maxKey = rel.supTuple();

		// populate the constant part
		for (Pair<Integer, Long> c : consts) {
			minKey.set(c.getLeft(), c.getRight());
			maxKey.set(c.getLeft(), c.getRight());
		}
	}

	public void eval() {
		// set an index for the relation
		rel.setIndex(index);

		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			minKey.set(c.getLeft(), t.get(c.getRight()));
			maxKey.set(c.getLeft(), t.get(c.getRight()));
		}

		SortedSet<Tuple> tuples = rel.lookup(minKey, maxKey);
		for (Tuple r : tuples) {
			for (Pair<Integer, Integer> p : assign) {
				t.set(p.getRight(), r.get(p.getLeft()));
			}
			cont.eval();
		}
	}

	@Override public String prettyPrint(int indent) {
		String s = Util.indent(indent) + String.format("FOR t IN %s WHERE ", rel.getName());

		for (Pair<Integer, Integer> t : test) {
			s += String.format("%s[%d] = t[%d] ", rel.getName(), t.getLeft(), t.getRight());
		}

		for (Pair<Integer, Long> c : consts) {
			s += String.format("%s[%d] = t[%d] ", rel.getName(), c.getLeft(), c.getRight());
		}

		for (Pair<Integer, Integer> a : assign) {
			s += String.format("t[%d] := %s[%d] ", a.getRight(), rel.getName(), a.getLeft());
		}

		return s + "\n" + cont.prettyPrint(indent + 1);
	}

	@Override public String toString() {
		return prettyPrint(0);
	}
}

class IfNotExists implements Control {
	private Tuple t;
	private Relation2 rel;
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Long>> consts;
	private Control cont;
	private Tuple minKey, maxKey;
	private Index index;


	IfNotExists(Tuple t, Relation2 rel, List<Pair<Integer, Integer>> test,
				List<Pair<Integer, Long>> consts, Control cont) {
		this.t = t;
		this.rel = rel;
		this.cont = cont;
		this.consts = consts;
		this.test = test;
		index = new Index(Stream.concat(test.stream(), consts.stream())
						  .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());

		this.minKey = rel.infTuple();
		this.maxKey = rel.supTuple();
		// populate the constant part
		for (Pair<Integer, Long> c : consts) {
			minKey.set(c.getLeft(), c.getRight());
			maxKey.set(c.getLeft(), c.getRight());
		}
	}

	public void eval() {
		rel.setIndex(index);
		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			minKey.set(c.getLeft(), t.get(c.getRight()));
			maxKey.set(c.getLeft(), t.get(c.getRight()));
		}

		SortedSet<Tuple> tuples = rel.lookup(minKey, maxKey);
		if (tuples.isEmpty())
			cont.eval();
	}

	@Override public String prettyPrint(int indent) {
		String s = Util.indent(indent) + String.format("IF (");
		for (Pair<Integer, Integer> t : test) {
			s += String.format("%d:t[%d], ", t.getLeft(), t.getRight());
		}

		for (Pair<Integer, Long> c : consts) {
			s += String.format("%d:%d, ", c.getLeft(), c.getRight());
		}

		s += ") IN " + rel.getName() + " THEN\n" + cont.prettyPrint(indent + 1);
		return s + "\n" + cont.prettyPrint(indent + 1);
	}

	@Override public String toString() {
		return prettyPrint(0);
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

	@Override public void eval() {
		Tuple u = new Tuple(rel.arity());

		for (Pair<Integer, Integer> a : assign) {
			u.set(a.getLeft(), t.get(a.getRight()));
		}

		for (Pair<Integer, Long> c : consts) {
			u.set(c.getLeft(), c.getRight());
		}

		rel.insert(u);

		cont.eval();
	}

	@Override public String prettyPrint(int indent) {
		String s = Util.indent(indent) + String.format("INSERT (");

		for (Pair<Integer, Integer> a : assign) {
			s += String.format("%d:t[%d], ", a.getLeft(), a.getRight());
		}

		for (Pair<Integer, Long> c : consts) {
			s += String.format("%d:%d, ", c.getLeft(), c.getRight());
		}

		s += ") INTO " + rel.getName() + "\n" + cont.prettyPrint(indent + 1);

		return s;
	}

	@Override public String toString() {
		return prettyPrint(0);
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

	@Override public String toString() {
		return prettyPrint(0);
	}

	protected abstract boolean test(long a, long b);
}
