public class Test {
	@ExpectWarning("DM_NUMBER_CTOR")
	public void test1() {
		new java.lang.Integer(1);
	}

	@ExpectWarning("DM_NUMBER_CTOR")
	public void test2() {
		new Float(2);
	}

	@ExpectWarning("DM_NUMBER_CTOR")
	public void test3() {
		new Double("3.0");
	}
}
