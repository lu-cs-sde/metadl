public class C {
	// Implements clone, but does not declare the Cloneable interface
	public Object clone() {
		return super.clone();
	}
}


class D {
	// Implements clone, but does not declare the Cloneable interface
	public Object clone() {
		super.clone();
		return null;
	}
}
