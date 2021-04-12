// Addapted from ErrorProne@7e8edd34c60dc7ae4ec18d18b84d4497ebf3ef9c

// public void negative()
// public void incompleteClasspath()
class Super {}
class Inner extends Super {}
class Test {
	void m() {
		new Inner();
	}
}

class Test2 {
	String foo(char[] cs) {
		return new String(cs);
	}

	String bar(byte[] bs) {
		return new String(bs);
	}
}
