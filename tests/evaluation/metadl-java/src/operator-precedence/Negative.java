// Addapted from ErrorProne@7e8edd34c60dc7ae4ec18d18b84d4497ebf3ef9c

class Test {
	int f(int a, int b) {
		int r = a + a * b;
		return r;
	}
	boolean f(boolean a, boolean b) {
		boolean r = (a && b) || (!a && !b);
		r = (a = a && b);
		return r;
	}
}
