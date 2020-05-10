//  public void okFBound()
class Test {
	interface Foo<T> {}
	static <T extends Foo<T>> T doCast(Object o) { return (T) o; }
}

// public void okGenericFactory()
class Test2 {
	static <T> List<T> newList() { return null; }
}

// public void okWithParam()
class Test3 {
    static <T> T noop(T t) { return t; }
}

// public void okNotMyParam()
class Test4<T> {
	T noop(T t) { return t; }
}

// public void issue343()
interface Test5 {
	interface Visitor1<X, Y> {}
	interface Visitor2<X, Y> {}
	<R,
		R1 extends R,
				   R2 extends R,
							  X1 extends Exception,
										 X2 extends Exception,
													V extends Visitor1<R1, X1> & Visitor2<R2, X2>>
															  R accept_(V v) throws X1, X2;
}

//  public void classTypeParameter()
abstract class Test6<T> {
	abstract T get(String s);
}

// public void typeAnnotation()
import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
@Target(ElementType.TYPE_USE) @interface A {}
class Test7 {
	<T> T f(@A T x) {
		return x;
	}
}
