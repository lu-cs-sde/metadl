import org.apache.commons.lang3.ObjectUtils.Null;

class C1 {
	public boolean equals(Object other) {
		return false;
	}

	void foo() { }
}

class C2 extends C1 {
	// Missing override here
	void foo() {
		Object o = new Object();
		o.hashCode();
	}
}

class C3 extends C2 implements I12 {
	// Missing override here
	void foo1() {

	}

	// Missing override here
	public boolean equals(Object other) {
		return true;
	}
}

class C4 extends C3 {
	@Override
	void foo1() {
	}

	@Override
	public boolean equals(Object other) {
		return false;
	}
}
