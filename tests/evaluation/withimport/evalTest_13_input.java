class A {
	void f() {};
}

class B {
	void g() {};
}

public class C {
	void h1() {};
	public void h2() {};
	void h3() {};
}

class D {
	public int f (int x, int y) {
		int z = y;
		int t = x;
		int a;

		a = z;
		a = t;
		z = x;
		z = t;
		x = t;
		x = x;

		return a + x + y;
	}
}
