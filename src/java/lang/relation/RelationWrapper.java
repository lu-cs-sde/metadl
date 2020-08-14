package lang.relation;

import java.util.Collection;

import eval.EvaluationContext;
import eval.Relation2;
import eval.Tuple;
import lang.ast.IntConstant;
import lang.ast.IntegerType;
import lang.ast.PredicateRef;
import lang.ast.PredicateRefType;
import lang.ast.PredicateType;
import lang.ast.StringConstant;
import lang.ast.StringType;
import lang.ast.Type;

class TupleWrapper {
	private final Tuple t;
	private final PredicateType type;
	private final EvaluationContext ctx;

	private TupleWrapper(final EvaluationContext ctx, final Tuple t, final PredicateType type) {
		this.t = t;
		this.ctx = ctx;
		this.type = type;
	}
}

public class RelationWrapper {
	private final EvaluationContext ctx;
	private final Relation2 rel;
	private final PredicateType type;

	public RelationWrapper(final EvaluationContext ctx, final Relation2 rel, final PredicateType type) {
		this.ctx = ctx;
		this.rel = rel;
		this.type = type;
	}

	public long internalizeObject(Object obj, int i) {
		if (type.get(i) == IntegerType.get()) {
			if (obj instanceof Long) {
				return (Long)obj;
			} else if (obj instanceof Integer) {
				return (long)(Integer)obj;
			} else if (obj instanceof IntConstant) {
				return Long.parseLong(((IntConstant) obj).getNUMERAL());
			}
		} else if (type.get(i) == StringType.get()) {
			if (obj instanceof String) {
				return ctx.internalizeString((String) obj);
			} else if (obj instanceof StringConstant) {
				return ctx.internalizeString(((StringConstant) obj).getSTRING());
			}
		} else if (type.get(i) == PredicateRefType.get()) {
			if (obj instanceof PredicateRef) {
				return ctx.internalizeString(((PredicateRef) obj).getPRED_ID());
			}
		}

		throw new RuntimeException("Unknown object type to add to a relation" + obj.toString());
	}

	public void insertTuple(final Object ...  args) {
		if (args.length != rel.arity())
			throw new RuntimeException("Arity mismatch between inserted tuple's arity and the relation arity");

		final Tuple t = new Tuple(rel.arity());

		for (int i = 0; i < rel.arity(); ++i) {
			t.set(i, internalizeObject(args[i], i));
		}

		rel.insert(t);
	}

	public void insertPseudoTuple(PseudoTuple p) {
		if (p.arity() != rel.arity())
			throw new RuntimeException("Arity mismatch between inserted tuple's arity and the relation arity");

		final Tuple t = new Tuple(rel.arity());

		for (int i = 0; i < rel.arity(); ++i) {
			t.set(i, internalizeObject(p.coord(i), i));
		}

		rel.insert(t);
	}

	public void insertPseudoTuples(Collection<? extends PseudoTuple> ps) {
		for (PseudoTuple p : ps) {
			insertPseudoTuple(p);
		}
	}
}
