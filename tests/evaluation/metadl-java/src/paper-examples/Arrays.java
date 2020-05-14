class Arrays {
    Object[] a = new Object[2];
    Object[] b = new Object[]{null, null};
    Object[] c = new Object[]{null, null};


    void foo() {
	Object c[] = new Object[]{a, b, c};
	Object d[] = new Object[]{a};
    }

    Object bar() {
	return c[2];
    }
}
