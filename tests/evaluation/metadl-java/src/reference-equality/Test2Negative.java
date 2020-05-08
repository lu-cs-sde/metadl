// public void negative_const()
class Foo {
}

class Test1 {
	public static final Foo CONST = new Foo();
	boolean f(Foo a) {
		return a == CONST;
	}
	boolean f(Object o, Test1 a) {
		// NO BUG, Object does not implement equals
		return o == a;
	}
}

//  public void negative_extends_equalsObject()
class Sup {
	public boolean equals(Object o) {
		return false;
	}
}

class Test2 extends Sup {
	boolean f(Object a, Test2 b) {
		// NO BUG, Object does not implement equals
		return a == b;
	}
}


//  public void negative_implementsInterface_equals()
interface Sup2 {
	public boolean equals(Object o);
}

class Test3 implements Sup2 {
	boolean f(Test3 a, Test3 b) {
		return a == b;
	}
}

//  public void negative_noEquals()
class Test4 {
	boolean f(Test4 a, Test4 b) {
		return a == b;
	}
}

//  public void negative_impl()
class Test5 {
	public boolean equals(Object o) {
		return this == o;
	}
}

//  public void negative_enum()
enum Enum5bis {
	A, B
}

class Test5bis {
	boolean f(Enum5bis a, Enum5bis b) {
		return a == b;
	}
}

//  public void test_customEnum()
enum Kind6 {
	FOO(42);
	private final int x;
	Kind6(int x) { this.x = x; }
}


class Test6 {
	boolean f(Kind6 a, Kind6 b) {
		return a == b;
	}
}

// public void negative_null()
class Test7 {
	boolean f(Optional<Integer> b) {
		return b == null;
	}
}

//  negative_abstractEq
interface Sup8 {
	public abstract boolean equals(Object o);
}

class Test8 implements Sup8 {
	boolean f(Object a, Test8 b) {
		return a == b;
	}
}


// public void negativeCase_class()
class Test9 {
	boolean f(String s) {
		return s.getClass() == String.class;
	}
}



// public void testErroneous()
class Missing {}

class MayImplementEquals {
	public void f(Missing m) {}
	public void g(Missing m) {}
}

abstract class Test10 {
	abstract MayImplementEquals getter();
	boolean f(MayImplementEquals b) {
		return getter() == b;
	}
}

//  public void negative_compareTo()
class Test11 implements Comparable<Test11> {
	public int compareTo(Test11 o) {
		return this == o ? 0 : -1;
	}
	public boolean equals(Object obj) {
		return obj instanceof Test11;
	}
	public int hashCode() {
		return 1;
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
