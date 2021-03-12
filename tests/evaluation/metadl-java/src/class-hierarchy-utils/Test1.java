class A {
	Object f() { return null; }

	static Object bar() { return null; }
}

class B extends A {
	B f() { return this; }

	static Object bar() { return null; }
}

interface I {
	int foo();
	int foo(int x);
	int foo(Object x);


}

interface J extends I {
}

class C extends B implements I {
	public int foo() { return 0;}
	public int foo(int x) { return 0;}
	public int foo(Object x) { return 0;}
}

class D<T> {

}

class E<T> extends D<T> implements I {
	public int foo() { return 0;}
	public int foo(int x) { return 0;}
	public int foo(Object x) { return 0;}
}

enum F {
	A {
		@Override
		public int fee() { return 1; }
	}

	;

	public int fee() {
		return 5;
	}
}

class Test1 {
	public static void main(String[] args) {
		//int x = F.A.foo();

	    I i = new I() {
				public int foo() {
					return 0;
				}
				public int foo(int x) { return 0;}
				public int foo(Object x) { return 0;}

			};
	}
}
