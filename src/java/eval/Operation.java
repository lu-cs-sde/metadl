package eval;

public interface Operation {
	long eval();

	public static Operation add(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() + r.eval();
			}
		};
	}

	public static Operation sub(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() - r.eval();
			}
		};
	}

	public static Operation mul(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() * r.eval();
			}
		};
	}

	public static Operation div(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() / r.eval();
			}
		};
	}

	public static Operation mod(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() % r.eval();
			}
		};
	}

	public static Operation constant(long c) {
		return new Operation() {
			@Override public long eval() {
				return c;
			}
		};
	}

	public static Operation component(Tuple t, int i) {
		return new Operation() {
			@Override public long eval() {
				return t.get(i);
			}
		};
	}

	// TODO: implement cat
}
