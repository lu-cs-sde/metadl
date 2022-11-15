package clang;

import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.function.Supplier;

import lang.c.obj.ast.ASTNode;
import lang.c.obj.ast.List;
import lang.c.obj.ast.Opt;


public interface ASTTrans<S, T>  {
	T trans(S s);

	static <W, T extends ASTNode, S extends ASTNode> ASTTrans<W, List<S>> map(ASTTrans<T, S> f, ASTTrans<W, Iterable<T>> lt) {
		return new ASTTrans<W, List<S>>() {
			@Override public List<S> trans(W w) {
				List<S> ret = new List<>();
				for (T e : lt.trans(w)) {
					if (e != null) {
						ret.add(f.trans(e));
					}
				}
				return ret;
			}
		};
	}

	default public <U> ASTTrans<U, T> after(ASTTrans<U, S> tu) {
		ASTTrans<S, T> outer = this;
		return new ASTTrans<U, T>(){
			@Override public T trans(U u) {
				S s = tu.trans(u);
				if (s == null)
					return null;
				return outer.trans(s);
			}
		};
	}

	static <S, T extends ASTNode> ASTTrans<S, Opt<T>> emptyOpt() {
		return new ASTTrans<S, Opt<T>>() {
			@Override public Opt<T> trans(S s) {
				return new Opt<T>();
			}
		};
	}

	static <S, T extends ASTNode> ASTTrans<S, Opt<T>> opt() {
		return (S ignore) -> new Opt<T>();
	}

	static <S, T extends ASTNode> ASTTrans<S, Opt<T>> opt(Supplier<T> f) {
		return (S ignore) -> {
			T v = f.get();
			return v == null ? new Opt<T>() : new Opt<T>(f.get());
		};
	}

	static <S, T extends ASTNode> ASTTrans<S, Opt<T>> opt(ASTTrans<S, T> tr) {
		return (S s) -> {
			T v = tr.trans(s);
			if (v != null)
				return new Opt<T>(v);
			return new Opt<T>();
		};
	}

	static <S, T extends ASTNode> ASTTrans<S, List<T>> list() {
		return (S ignore) -> new List<T>();
	}


	static <S, T extends ASTNode> ASTTrans<S, List<T>> list(Supplier<T> f) {
		return (S ignore) -> new List<T>().add(f.get());
	}



	static <S, T extends ASTNode> ASTTrans<S, List<T>> emptyList() {
		return new ASTTrans<S, List<T>>() {
			@Override public List<T> trans(S s) {
				return new List<T>();
			}
		};
	}

	static <S, T> ASTTrans<S, T> node(Supplier<T> f) {
		return (S ig) -> f.get();
	}

	static <S, T, U, V> ASTTrans<S, V> node(BiFunction<T, U, V> f, ASTTrans<S, T> t1, ASTTrans<S, U> t2) {
		return (S s) -> f.apply(t1.trans(s), t2.trans(s));
	}


	static <T extends ASTNode, S extends ASTNode, U, V> ASTTrans<U, List<T>> append(ASTTrans<U, List<T>> tl, Supplier<T> s) {
		return new ASTTrans<U, List<T>>() {
			@Override public List<T> trans(U u) {
				List<T> ret = tl.trans(u);
				T e = s.get();
				if (e != null)
					ret.add(e);
				return ret;
			}
		};
	}

	static <T, U, V> Match<T, U, V> match(ASTTrans<T, U> tr) {
		Match<T, U, V> m = new Match<T, U, V>(tr);
		return m;
	}

	static class Match<T, U, V> {
		ASTTrans<T, U> tr;

		Match(ASTTrans<T, U> tr) {
			this.tr = tr;
		}

		ASTTrans<U, V> onMatch(U u) {
			return null;
		}

		public Match<T, U, V> of(Predicate<U> p, ASTTrans<U, V> uv) {
			Match<T, U, V> outer = this;
			return new Match<T, U, V>(tr) {
				@Override ASTTrans<U, V> onMatch(U u) {
					ASTTrans<U, V> outerUV = outer.onMatch(u);
					if (outerUV != null) {
						return outerUV;
					} else if (p.test(u)) {
						return uv;
					} else {
						return null;
					}
				}
			};
		}

		public ASTTrans<T, V> otherwise(ASTTrans<U, V> tr1) {
			return new ASTTrans<T, V>() {
				@Override public V trans(T t) {
					U u = tr.trans(t);
					ASTTrans<U, V> uv = onMatch(u);
					if (uv != null) {
						return uv.trans(u);
					} else {
						return tr1.trans(u);
					}
				}
			};
		}
	}

	interface Function4<T0, T1, T2, T3, R> {
		R apply(T0 t0, T1 t1, T2 t2, T3 t3);
	}

	interface Function3<T0, T1, T2, R> {
		R apply(T0 t0, T1 t1, T2 t2);
	}

}
