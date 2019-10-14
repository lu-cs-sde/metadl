interface I {}

interface J extends I {
	int f();
}
interface H extends J {
	int g();
}

abstract class A {}

class B extends A implements H {
	public int f() {
		return 1;
	}
	public int g() {
		return 0;
	}
}

class C extends B {
	public void h() {
	}
}
