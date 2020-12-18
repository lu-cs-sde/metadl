package eval;

public interface Operation {
	long eval();
	String prettyPrint();

	public static Operation add(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() + r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " + " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation sub(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() - r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " - " + r.prettyPrint() + ")";
			}

		};
	}

	public static Operation mul(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() * r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " * " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation div(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() / r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " / " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation mod(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() % r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " % " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation bor(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() | r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " bor " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation band(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() & r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " band " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation bxor(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() % r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " bxor " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation bshl(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() << r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " bshl " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation bshr(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() >> r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " bshr " + r.prettyPrint() + ")";
			}
		};
	}

	public static Operation bshru(Operation l, Operation r) {
		return new Operation() {
			@Override public long eval() {
				return l.eval() >>> r.eval();
			}

			@Override public String prettyPrint() {
				return "(" + l.prettyPrint() + " bshru " + r.prettyPrint() + ")";
			}
		};
	}



	public static Operation constant(long c) {
		return new Operation() {
			@Override public long eval() {
				return c;
			}

			@Override public String prettyPrint() {
				return "" + c;
			}

		};
	}

	public static Operation component(Tuple t, int i) {
		return new Operation() {
			@Override public long eval() {
				return t.get(i);
			}

			@Override public String prettyPrint() {
				return "t[" + i + "]";
			}
		};
	}

	public static Operation to_number(EvaluationContext ctx, Operation arg) {
		return new Operation() {
			@Override public long eval() {
				String s = ctx.externalizeString(arg.eval());
				return Long.parseLong(s);
			}

			@Override public String prettyPrint() {
				return "to_number(" + arg.prettyPrint() + ")";
			}
		};
	}

	public static Operation cat(EvaluationContext ctx, Operation arg0, Operation arg1) {
		return new Operation() {
			@Override public long eval() {
				String s0 = ctx.externalizeString(arg0.eval());
				String s1 = ctx.externalizeString(arg1.eval());
				return ctx.internalizeString(s0 + s1);
			}

			@Override public String prettyPrint() {
				return "cat(" + arg0.prettyPrint() + ", " + arg1.prettyPrint() + ")";
			}
		};
	}
}
