class C {
	// The field is written in another class
	@NoWarning("UWF_UNWRITTEN_FIELD")
	Object externalObject;

	public Object anotherBug() {
		return externalObject;
	}
}

class D {
	{
		C c = new C();
		c.externalObject = null;
	}
}


class F {
	@NoWarning("UWF_UNWRITTEN_FIELD")
	int x;
	@NoWarning("UWF_UNWRITTEN_FIELD")
	int y;
	@ExpectWarning("UWF_UNWRITTEN_FIELD")
	int z;

	void f() {
		x++;
	}

	void g() {
		--this.y;
	}

	void h() {
		x <<= z;
	}
}
