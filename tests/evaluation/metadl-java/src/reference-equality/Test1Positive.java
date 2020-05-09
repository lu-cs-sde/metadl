// Adapted from ErroProne's
// core/src/test/java/com/google/errorprone/bugpatterns/ReferenceEqualityTest.java

//   public void protoGetter_nonnull()
class ClassWithEquals {
    public boolean equals(Object o) {
	return true;
    }
}

class Super {
    public boolean equals(Object o) {
	return false;
    }
}

class GenericClass<T> extends Super {
    T a;
    T get(int i) {
	return a;
    }
}

class Foo {
    void something(GenericClass<ClassWithEquals> f1, GenericClass<ClassWithEquals> f2) {
	// BUG: Super :> GenericClass overrides Object.equals()
	boolean b = f1 == f2;
	// BUG: String overrides Object.equals(). This is found
	// only if we allow MetaDL to dive into the JavaRT library
	b = f1.get(0) == f2.get(0);
    }
}

// public void positive_extendsAbstract_equals()
abstract class Sup {
    public abstract boolean equals(Object o);
}

abstract class Test1 extends Sup {
    boolean f(Test1 a, Test1 b) {
	// BUG: Diagnostic contains: a.equals(b),
	return a == b;
    }
}

// public void positive_equal() {
class Optional<T> {
    T t;
    public static T makeJust(T t) { Optional<T> o = new Optional(); o.t = t; }
    public static T makeNothing() { Optional<T> o = new Optional(); o.t = null; }

    public boolean equals(Object o) {
	if (o == this) return true;
	if (!(o instanceof Optional)) return false;
	return t.equals((Optional) o);
    }
}

class Test2 {
    boolean f(Optional<Integer> a, Optional<Integer> b) {
	// BUG: Diagnostic contains: a.equals(b)
	return a == b;
    }
}

// public void positive_equalWithOr()
class Test3 {
    boolean f(Optional<Integer> a, Optional<Integer> b) {
	// BUG
	return a == b || (a.equals(b));
    }
}

// public void positive_equalWithOr_objectsEquals()
class Test4 {
    boolean f(Optional<Integer> a, Optional<Integer> b) {
	// BUG
	boolean eq = a == b || Objects.equal(a, b);
	// BUG
	return a == b || (java.util.Objects.equals(a, b));
    }
}

// public void positive_notEqual()
class Test5 {
    boolean f(Optional<Integer> a, Optional<Integer> b) {
	// BUG: Diagnostic contains: !a.equals(b)
	return a != b;
    }
}

// public void transitiveEquals()
public class Super6 {
    public boolean equals(Object o) {
	return false;
    }
}
public class Mid6 extends Super6 { }
public class Sub6 extends Mid6 { }
abstract class Test6 {
    boolean f(Sub6 a, Sub6 b) {
	// BUG: Diagnostic contains: a.equals(b)
	return a == b;
    }
}

//  public void typaram()
class Test7<T extends ClassWithEquals, X> {
    boolean f(T t) {
	return t == null;
    }
    boolean g(T t1, T t2) {
	// BUG: Diagnostic contains:
	return t1 == t2;
    }
    boolean g(X x1, X x2) {
	return x1 == x2;
    }
}

//  public void likeCompareToButDifferentName()
class Test8 implements Comparable<Test8> {
    public int compareTo(Test8 o) {
	return this == o ? 0 : -1;
    }
    public int notCompareTo(Test8 o) {
	// BUG: Diagnostic contains:
	return this == o ? 0 : -1;
    }
    public boolean equals(Object obj) {
	return obj instanceof Test;
    }
    public int hashCode() {
	return 1;
    }
}

//  public void positive_compareTo()
class Test9 implements Comparable<String> {
    String f;
    public int compareTo(String o) {
	// BUG: Diagnostic contains:
	return f == o ? 0 : -1;
    }
}
