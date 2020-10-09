package lang.relation;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

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


public class RelationWrapper implements TupleInserter {
	public static class TupleWrapper {
		private final Tuple t;
		private final PredicateType type;
		private final EvaluationContext ctx;

		TupleWrapper(final EvaluationContext ctx, final Tuple t, final PredicateType type) {
			this.t = t;
			this.ctx = ctx;
			this.type = type;
		}

		@Override public String toString() {
			String s = "(";
			for (int i = 0; i < t.arity(); ++i) {
				if (i != 0) {
					s += ", ";
				}
				if (type.get(i).storageType() == IntegerType.get()) {
					s += t.get(i);
				} else if (type.get(i).storageType() == StringType.get()) {
					s += "\"" + ctx.externalizeString(t.get(i)) + "\"";
				} else {
					assert type.get(i).storageType() == PredicateRefType.get();
					s += "'" + ctx.externalizeString(t.get(i));
				}
			}
			return s + ")";
		}


		@Override public boolean equals(Object o) {
			if (!(o instanceof TupleWrapper))
				return false;
			TupleWrapper other = (TupleWrapper) o;

			if (t.arity() != other.t.arity())
				return false;

			for (int i = 0; i < t.arity(); ++i) {
				if (type.get(i) == IntegerType.get() &&
					t.get(i) != other.t.get(i)) {
					return false;
				} else if ((type.get(i) == StringType.get() || type.get(i) == PredicateRefType.get()) &&
						   !ctx.externalizeString(t.get(i)).equals(other.ctx.externalizeString(other.t.get(i)))) {
					return false;
				}
			}
			return true;
		}

		@Override public int hashCode() {
			return toString().hashCode();
		}

		public long getAsLong(int i) {
			if (type.get(i) != IntegerType.get())
				throw new RuntimeException("Component is not of integer type");
			return t.get(i);
		}

		public String getAsString(int i) {
			if (type.get(i) != StringType.get() &&
				type.get(i) != PredicateRefType.get())
				throw new RuntimeException("Component is not of String or PredicateRef type");
			return ctx.externalizeString(t.get(i));
		}
	}


	private final EvaluationContext ctx;
	private final Relation2 rel;
	private final PredicateType type;

	public RelationWrapper(final EvaluationContext ctx, final Relation2 rel, final PredicateType type) {
		this.ctx = ctx;
		this.rel = rel;
		this.type = type;
	}

	public long internalizeObject(Object obj, int i) {
		if (type.get(i).storageType() == IntegerType.get()) {
			if (obj instanceof Long) {
				return (Long)obj;
			} else if (obj instanceof Integer) {
				return (long)(Integer)obj;
			} else if (obj instanceof IntConstant) {
				return Long.parseLong(((IntConstant) obj).getNUMERAL());
			}
		} else if (type.get(i).storageType() == StringType.get()) {
			if (obj instanceof String) {
				return ctx.internalizeString((String) obj);
			} else if (obj instanceof StringConstant) {
				return ctx.internalizeString(((StringConstant) obj).getSTRING());
			}
		} else if (type.get(i).storageType() == PredicateRefType.get()) {
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

	public void insertTuple(TupleWrapper w) {
		if (w.t.arity() != rel.arity())
			throw new RuntimeException("Arity mismatch between inserted tuple's arity and the relation arity");
		Tuple t = new Tuple(rel.arity());

		for (int i = 0; i < w.t.arity(); ++i) {
			if (type.get(i).storageType() != w.type.get(i).storageType())
				throw new RuntimeException("Type mismatch between inserted tuple and relation at component " + i + ".");
			if (type.get(i).storageType() == IntegerType.get()) {
				t.set(i, w.t.get(i));
			} else {
				assert type.get(i).storageType() == StringType.get() ||
					type.get(i).storageType() == PredicateRefType.get();
				t.set(i, ctx.internalizeString(w.ctx.externalizeString(w.t.get(i))));
			}
		}

		rel.insert(t);
	}

	public void insertTuples(Collection<? extends TupleWrapper> ts) {
		for (TupleWrapper w : ts) {
			insertTuple(w);
		}
	}

	public PredicateType type() {
		return type;
	}

	public Relation2 getRelation() {
		return rel;
	}

	public EvaluationContext getContext() {
		return ctx;
	}

	public Set<TupleWrapper> tuples() {
		return rel.tuples().stream().map(p -> new TupleWrapper(ctx, p, type)).collect(Collectors.toSet());
	}
}
