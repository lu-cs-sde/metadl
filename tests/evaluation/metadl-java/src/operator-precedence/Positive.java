// Addapted from ErrorProne@7e8edd34c60dc7ae4ec18d18b84d4497ebf3ef9c

//  public void positive() {
class Test {
    boolean f(boolean a, boolean b, boolean c) {
	// BUG: Diagnostic contains: (a && b) || c
	boolean r = a && b || c;
	// BUG: Diagnostic contains: a || (b && c)
	r = a || b && c;
	// BUG: Diagnostic contains: a || (b && c) || !(b && c)
	r = a || b && c || !(b && c);
	return r;
    }
    int f(int a, int b) {
	// BUG: Diagnostic contains: (a + b) << 2
	return a + b << 2;
    }
}

//  public void positiveNotSpecialParenthesisCase() {
class Test2 {
    boolean f(boolean a, boolean b, boolean c, boolean d, boolean e) {
	// BUG, EXTRAPAREN, EXTRAPAREN
	boolean r = a || (b && c) && (d && e);
	return r;
    }
    int f2(int a, int b, int c, int d) {
	// BUG
	int e = a << (b + c) + d;
	return e;
    }
    boolean f3(boolean a, boolean b, boolean c, boolean d, boolean e) {
	// BUG
	boolean r = a || b && c;
	return r;
    }
}


// public void extraParenthesis()
class Test3 {
    void f(boolean a, boolean b, boolean c, boolean d, boolean e) {
	// EXTRAPAREN, BUG
	boolean g = (a || (b && c && d) && e);
    }
}

//  public void rightAndParenthesis()
class Test4 {
    void f(boolean a, boolean b, boolean c, boolean d) {
	// BUG, EXTRAPAREN
	boolean g = a || b && (c && d);
    }
}

// public void leftAndParenthesis()
class Test5 {
    void f(boolean a, boolean b, boolean c, boolean d) {
	// BUG, EXTRAPAREN
	boolean g = a || (b && c) && d;
    }
}

// public void aLotOfParenthesis()
class Test6 {
    void f(boolean a, boolean b, boolean c, boolean d, boolean e) {
	// BUG, EXTRAPAREN
	boolean g = (a || (b && c && d) && e);
    }
}

class Test7 {
	Test7 f() {
		// BUG, EXTRAPAREN
		return (new Test7());
	}

	int g() {
		// BUG, EXTRAPAREN
		return (g());
	}

	int h() {
		// BUG, EXTRAPAREN
		int t[] = new int[3];
		return (t[1]);

	}
}
