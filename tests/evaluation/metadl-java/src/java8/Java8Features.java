import java.util.function.UnaryOperator;

class Java8Features {
	void lambdaForAll(List<String> l) {
		l.forAll(s -> System.out.println(s));
	}

	<T> T apply(T t, UnaryOperator<T> f) {
		return f.apply(t);
	}

	Integer inc(Integer t) {
		return t + 1;
	}

	void testApply() {
		Integer t = 5;
		UnaryOperator<Integer> uop = this::inc;
		apply(t, uop);
	}
}
