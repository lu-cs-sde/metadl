package lang.relation;

import java.util.Collection;

import eval.EvaluationContext;
import eval.Relation2;
import eval.Tuple;
import lang.ast.IntConstant;
import lang.ast.StringConstant;

class TupleWrapper {
	private final Tuple t;
	private final EvaluationContext ctx;

	public TupleWrapper(final EvaluationContext ctx, final Tuple t) {
		this.t = t;
		this.ctx = ctx;
	}

	public long getAsLong(final int i) {
		return t.get(i);
	}

	public String getAsString(final int i) {
		return ctx.externalizeString(t.get(i));
	}
}

public class RelationWrapper {
	private final EvaluationContext ctx;
	private final Relation2 rel;

	public RelationWrapper(final EvaluationContext ctx, final Relation2 rel) {
		this.ctx = ctx;
		this.rel = rel;
	}

	public void insertTuple(final Object ...  args) {
		if (args.length != rel.arity())
			throw new RuntimeException("Arity mismatch between inserted tuple's arity and the relation arity");

		final Tuple t = new Tuple(rel.arity());

		for (int i = 0; i < rel.arity(); ++i) {
			if (args[i] instanceof Long) {
				t.set(i, (Long)args[i]);
			} else if (args[i] instanceof Integer) {
				t.set(i, (long)(Integer)args[i]);
			} else if (args[i] instanceof IntConstant) {
				t.set(i, Long.parseLong(((IntConstant) args[i]).getNUMERAL()));
			} else if (args[i] instanceof String) {
				t.set(i, ctx.internalizeString((String) args[i]));
			} else if (args[i] instanceof StringConstant) {
				t.set(i, ctx.internalizeString(((StringConstant) args[i]).getSTRING()));
			} else {
				throw new RuntimeException("Unknown argument type for insertTuple, " + args[i].toString());
			}
		}

		rel.insert(t);
	}

	public void insertPseudoTuple(PseudoTuple p) {
		if (p.arity() != rel.arity())
			throw new RuntimeException("Arity mismatch between inserted tuple's arity and the relation arity");

		final Tuple t = new Tuple(rel.arity());

		for (int i = 0; i < rel.arity(); ++i) {
			if (p.coord(i) instanceof IntConstant) {
				t.set(i, Long.parseLong(((IntConstant) p.coord(i)).getNUMERAL()));
			} else if (p.coord(i) instanceof StringConstant) {
				t.set(i, ctx.internalizeString(((StringConstant) p.coord(i)).getSTRING()));
			} else {
				throw new RuntimeException("Unknown argument type for insertTuple, " + p.coord(i).toString());
			}
		}

		rel.insert(t);
	}

	public void insertPseudoTuples(Collection<? extends PseudoTuple> ps) {
		for (PseudoTuple p : ps) {
			insertPseudoTuple(p);
		}
	}
}
