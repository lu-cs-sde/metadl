// Addapted from ErrorProne@7e8edd34c60dc7ae4ec18d18b84d4497ebf3ef9c

// public void negative()
class Test1 {
	{
		String s = new String((String) null);
	}
}

// public void incompleteClasspath()
class Super {}
class Inner extends Super {}
class Test {
	void m() {
		new Inner();
	}
}
