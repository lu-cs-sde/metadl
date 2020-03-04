class C {

	int z;

	int h() {
		{
			int z;
			z = 5;
		}

		{
			int z;
			z = 6;
		}

		return 0;
	}

	int g() {
		return z;
	}

	class D {
		int q;
		class E {
			int q;
			int m() {
				return q + z;
			}
		}
	}
}
