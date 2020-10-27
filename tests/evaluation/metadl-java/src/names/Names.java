class C {
	static String a;
	static String b;
	String x, y;
	class Inner {
		Integer i;
		Integer j;
	}

	public void foo() {
	}
}

public class Names {
	void foo() {
		C.a = C.b;
		C c = new C();
		C.Inner t = c.new Inner();
		c.x = c.y;
		c.foo();
	}
}
