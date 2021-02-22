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
	// Missing override here is OK, since
	// there will be a compiler error if the class
	// does not implement the method.
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

class C5 extends C3 {
	void foo1(int x) {
	}

	public boolean equals(Object other, Object arg2) {
		return false;
	}
}

class C6 implements I4 {
	void foo3(String[] x) {
	}
}

abstract class C7 {
	abstract void foo4();
}

class C8 extends C7 {
	void foo4() {
	}
}

class C9 implements Comparable<C9> {
	public boolean equals(Object o) {
		return false;
	}

	public int compareTo(C9 c9) {
		return 0;
	}
}
