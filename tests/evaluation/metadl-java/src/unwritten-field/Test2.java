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
