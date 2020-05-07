class Sup {
	public boolean equals(Object o) {
		return false;
	}
}

class Test extends Sup {
	boolean f(Object a, Test b) {
		// NO BUG, Object does not implement equals
		return a == b;
	}
}


class TestPrime {
	public boolean equals(Object o) {
		// NO BUG, we're comparing against this
		if (o == this) {
			return true;
		}
		// do something else...
		return false;
	}
}
