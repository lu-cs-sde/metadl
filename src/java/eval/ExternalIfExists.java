package eval;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

public abstract class ExternalIfExists implements Control {
	private EvaluationContext ctx;
	private int arity;
	private String name;
	private List<Pair<Integer, Integer>> test;
	private List<Pair<Integer, Long>> consts;
	private Control cont;
	private final boolean positive;
	private List<Integer> argIdx;

	public ExternalIfExists(EvaluationContext ctx,
					 int arity,
					 String name,
					 List<Pair<Integer, Integer>> test,
					 List<Pair<Integer, Long>> consts,
					 Control cont,
					 boolean positive) {
		this.ctx = ctx;
		this.arity = arity;
		this.name = name;
		this.cont = cont;
		this.consts = consts;
		this.test = test;
		this.positive = positive;
		this.argIdx = Stream.concat(test.stream(), consts.stream())
			.map(p -> p.getLeft()).sorted().collect(Collectors.toList());

	}

	@Override public void eval(Tuple t) {
		Tuple in = new Tuple(arity);


		// populate the constant part
		for (Pair<Integer, Long> c : consts) {
			in.set(c.getLeft(), c.getRight());
		}

		// populate the variable part of the prefix
		for (Pair<Integer, Integer> c : test) {
			in.set(c.getLeft(), t.get(c.getRight()));
		}

		if (positive ^ !exists(t))
			cont.eval(t);
	}

	protected List<Integer> argIdx() {
		return this.argIdx;
	}

    protected abstract boolean exists(Tuple t);

	protected EvaluationContext getContext() {
		return this.ctx;
	}

	@Override public String prettyPrint(int indent) {
		String s = Util.indent(indent) + String.format("EXTERNAL IF (");
		for (Pair<Integer, Integer> t : test) {
			s += String.format("%d:t[%d], ", t.getLeft(), t.getRight());
		}

		for (Pair<Integer, Long> c : consts) {
			s += String.format("%d:%d, ", c.getLeft(), c.getRight());
		}

		s += ") " + (positive ? "" : "NOT") + " IN " + name + " THEN\n" + cont.prettyPrint(indent + 1);
		return s;
	}

	@Override public String toString() {
		return prettyPrint(0);
	}
}
