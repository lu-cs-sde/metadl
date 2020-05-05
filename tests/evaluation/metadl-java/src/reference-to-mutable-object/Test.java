class Test {
	// Check that fields with value types are not
	// considered exposed if their value is returned.

	private int i = 0;

	@NoWarning("EI_EXPOSE_REP")
	public int exposeInternal() {
		return i;
	}

	@NoWarning("EI_EXPOSE_REP2")
	public void setInternal(int v) {
		this.i = v;
	}
}
