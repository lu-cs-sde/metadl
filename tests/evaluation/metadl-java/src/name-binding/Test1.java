import java.util.ArrayList;

class Test2 {
    int t;
    Object o1;
    java.lang.Object o2, o3;

    public void f() {
	int t;
	int x;
    }

    public void g() {
	class C {

	}

	for (int i = 0; i < 5; ++i) {
	}

	ArrayList<ArrayList<String>> l = null;
	for (ArrayList<String> s : l) {

	}
    }
}

public enum Test1 {
    ENUM_1,
    ENUM_2;

}

class A {
    private class B {
    }
}

interface I {
    public void ifm();
    class A {
	public void func() {};
    }
}
