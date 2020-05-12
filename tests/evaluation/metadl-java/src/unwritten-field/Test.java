class C {
	@NoWarning("UWF_UNWRITTEN_FIELD")
	private String NOBUG1;

	@NoWarning("UWF_UNWRITTEN_FIELD")
	private String NOBUG2;

	@ExpectWarning("UWF_UNWRITTEN_FIELD")
	private String BUG;

	public void WriteNoBug1() {
		NOBUG1 = "Text";
	}

	public String ReadNoBug1() {
		return this.NOBUG1;
	}

	public void WriteNoBug2() {
		this.NOBUG2 = "Text";
	}

	public String ReadNoBug2() {
		return NOBUG2;
	}

	public String Bug() {
		return this.BUG;
	}

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
