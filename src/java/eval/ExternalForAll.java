package eval;

import java.util.List;
import java.util.SortedSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

public abstract class ExternalForAll implements Control {
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Integer>> assign;
	private List<Pair<Integer, Long>> consts;
	private List<Integer> argIdx;
	private List<Integer> resIdx;
	private EvaluationContext ctx;
	private int arity;
	private String name;
	private Control cont;

	/**
	   test - pairs of (column, tuple position) to test for equality
	   assign - pairs of (column, tuple position) to assign
	   consts - pairs of (column, value) of constants to assign
	   arity - the arity of the external relation
	 */
	public ExternalForAll(EvaluationContext ctx,
				   int arity,
				   String name,
				   List<Pair<Integer, Integer>> test,
				   List<Pair<Integer, Long>> consts,
				   List<Pair<Integer, Integer>> assign,
				   Control cont) {
		this.ctx = ctx;
		this.arity = arity;
		this.name = name;
		this.test = test;
		this.consts = consts;
		this.assign = assign;
		this.cont = cont;
		this.argIdx = Stream.concat(test.stream(), consts.stream())
			.map(p -> p.getLeft()).sorted().collect(Collectors.toList());

		this.resIdx = assign.stream().map(p -> p.getLeft()).distinct().sorted().collect(Collectors.toList());
	}

	@Override public void eval(Tuple t) {
		Tuple in = new Tuple(arity);
		for (Pair<Integer, Long> c : consts) {
			in.set(c.getLeft(), c.getRight());
		}

		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			in.set(c.getLeft(), t.get(c.getRight()));
		}

		Stream<Tuple> tuples = externalLookup(in);

		tuples.forEachOrdered(r -> {
				for (Pair<Integer, Integer> p : assign) {
					t.set(p.getRight(), r.get(p.getLeft()));
				}
				cont.eval(t);
			});
	}


	protected List<Integer> argIdx() {
		return this.argIdx;
	}

	protected List<Integer> resIdx() {
		return this.resIdx;
	}

	protected EvaluationContext getContext() {
		return this.ctx;
	}

	protected abstract Stream<Tuple> externalLookup(Tuple t);

	@Override public String prettyPrint(int indent) {
		String s = Util.indent(indent) + String.format("EXTERNAL FOR t IN %s WHERE ", name);

		for (Pair<Integer, Integer> t : test) {
			s += String.format("%s[%d] = t[%d] ", name, t.getLeft(), t.getRight());
		}

		for (Pair<Integer, Long> c : consts) {
			s += String.format("%s[%d] = %d ", name, c.getLeft(), c.getRight());
		}

		for (Pair<Integer, Integer> a : assign) {
			s += String.format("t[%d] := %s[%d] ", a.getRight(), name, a.getLeft());
		}

		return s + "\n" + cont.prettyPrint(indent + 1);

	}
}
