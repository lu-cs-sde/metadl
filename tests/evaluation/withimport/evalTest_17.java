class C {
	int g() { return 5; }
	int f() {
		int x;

		x = 1 * 2 + 3 * 4;
		return x;
	}

	int e() {
		int x, y;
		x = (y = 1);
		return x;
	}

	int h() {
		int w = 5;
		int x, y, z;
		x = 1;
		x = 2;
		y = 2 * x;
		z = x + y;
		z = z + z;
		w = x * y + z;
		z = w + (y = 3);
		return z;
	}

	public static void main(String args[]) {
		C c = new C();
		System.out.println(c.f());
		System.out.println(c.g());
		System.out.println(c.h());
		System.out.println(c.e());

	}
}
