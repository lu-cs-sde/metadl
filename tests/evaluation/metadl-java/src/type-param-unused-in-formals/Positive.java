
//  public void evilCastImpl()
class Test1 {
	  // BUG: Diagnostic contains:
	  static <T> T doCast(Object o) { T t = (T) o; return t; }
}

//  public void leadingParam()
class Test2 {
	// BUG: Diagnostic contains:
	static <U extends Object, T> T doCast(U o) { T t = (T) o; return t; }
}

//  public void trailingParam()
class Test3 {
  // BUG: Diagnostic contains:"
  static <T, U extends Object> T doCast(U o) { T t = (T) o; return t; }
}

//  public void leadingAndTrailingParam()
class Test4 {
	// BUG: Diagnostic
	static <V extends Object, T, U extends Object> T doCast(U o, V v) { T t = (T) o; return t; }
}

// public void superBound() {
class Test5 {
	// BUG: Diagnostic contains:",
	static <T extends Number> T doCast(Object o) { return (T) o; }
}

// public void wildbound()
class Test6 {
	interface Foo<T> {}
	// BUG: Diagnostic contains:
	static <T extends Foo<?>> T doCast(Object o) { return (T) o; }
}

// public void abstractMethod()
abstract class Test7 {
	// BUG: Diagnostic contains:
	abstract <T> T badMethod();
}

//  public void objectCast()
class Test8 {
	// BUG: Diagnostic contains:
	<T> T badMethod(String s) { return (T) s; }
}
