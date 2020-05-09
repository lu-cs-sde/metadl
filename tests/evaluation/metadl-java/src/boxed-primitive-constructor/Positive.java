// Addapted from ErrorProne@7e8edd34c60dc7ae4ec18d18b84d4497ebf3ef9c

//  public void positive()
class Test1 {
    // BUG: Diagnostic contains: byte b = (byte) 0;
    byte b = new Byte((byte) 0);
    // BUG: Diagnostic contains: char c = (char) 0;
    char c = new Character((char) 0);
    // BUG: Diagnostic contains: double d = 0;
    double d = new Double(0);
    // BUG: Diagnostic contains: float f = 0;
    float f = new Float(0);
    // BUG: Diagnostic contains: int i = 0;
    int i = new Integer(0);
    // BUG: Diagnostic contains: long j = 0;
    long j = new Long(0);
    // BUG: Diagnostic contains: short s = (short) 0;
    short s = new Short((short) 0);
    Double dd = d;
    // BUG: Diagnostic contains: float f2 = dd.floatValue();
    float f2 = new Float(dd);
    // BUG: Diagnostic contains: float f3 = (float) d;
    float f3 = new Float(d);
    // BUG: Diagnostic contains: foo(Float.valueOf((float) d));
    { foo(new Float(d)); }
    public void foo(Float f) {}
}

//  public void positiveStrings()
class Test2 {
    {
	// BUG: Diagnostic contains: byte b = Byte.valueOf(\0\);
	byte b = new Byte("0");
	// BUG: Diagnostic contains: double d = Double.valueOf(\0\);
	double d = new Double("0");
	// BUG: Diagnostic contains: float f = Float.valueOf(\0\);
	float f = new Float("0");
	// BUG: Diagnostic contains: int i = Integer.valueOf(\0\);
	int i = new Integer("0");
	// BUG: Diagnostic contains: long j = Long.valueOf("0");
	long j = new Long("0");
	// BUG: Diagnostic contains: short s = Short.valueOf("0");
	short s = new Short("0");
    }
}

// public void booleanConstant()
class Test3 {
    static final Boolean CONST = true;
    static final String CONST2 = null;
    {
        // BUG: Diagnostic contains: boolean a = true;
        boolean a = new Boolean(true);
        // BUG: Diagnostic contains: boolean b = false;
        boolean b = new Boolean(false);
        // BUG: Diagnostic contains: boolean c = Boolean.valueOf(CONST);
        boolean c = new Boolean(CONST);
        // BUG: Diagnostic contains: boolean e = true;
        boolean e = new Boolean("true");
        // BUG: Diagnostic contains: boolean f = false;
        boolean f = new Boolean("nope");
        // BUG: Diagnostic contains: boolean g = Boolean.valueOf(CONST2);
        boolean g = new Boolean(CONST2);
        // BUG: Diagnostic contains: System.err.println(Boolean.TRUE);
        System.err.println(new Boolean("true"));
        // BUG: Diagnostic contains: System.err.println(Boolean.FALSE);
        System.err.println(new Boolean("false"));
    }
}

//  public void autoboxing()
abstract class Test4 {
    abstract int g(Integer x);
    void f(int x) {
	// BUG: Diagnostic contains: int i = x;
	int i = new Integer(x);
	// BUG: Diagnostic contains: i = g(Integer.valueOf(x));
	i = g(new Integer(x));
	// BUG: Diagnostic contains: i = (short) 0;
	i = new Integer((short) 0);
    }
}

//  public void methodCall()
abstract class Test5 {
    abstract int g(Integer x);
    void f(int x) {
	// BUG: Diagnostic contains: int i = Integer.valueOf(x).byteValue();
	int i = new Integer(x).byteValue();
    }
}

//  public void stringValue()
abstract class Test6 {
    abstract int g(Integer x);
    void f(int x) {
	// BUG: Diagnostic contains: String s = String.valueOf(x);
	String s = new Integer(x).toString();
    }
}

//  public void compareTo()
abstract class Test7 {
    abstract int g(Integer x);
    void f(int x, Integer y, double d, Double dd, Float f) {
	// BUG: Diagnostic contains: int c1 = Integer.compare(x, y);
	int c1 = new Integer(x).compareTo(y);
	// BUG: Diagnostic contains: int c2 = y.compareTo(Integer.valueOf(x));
	int c2 = y.compareTo(new Integer(x));
	// BUG: Diagnostic contains: int c3 = Float.compare((float) d, f);
	int c3 = new Float(d).compareTo(f);
	// BUG: Diagnostic contains: int c4 = Float.compare(dd.floatValue(), f);
	int c4 = new Float(dd).compareTo(f);
    }
}

// public void testHashCode()
abstract class Test8 {
    abstract int g(Integer x);
    int f(int x, Integer y, long z, double d, Double dd) {
	// BUG: Diagnostic contains: int h = Integer.hashCode(x);
	int h = new Integer(x).hashCode();
	// BUG: Diagnostic contains: h = Float.hashCode((float) d);
	h = new Float(d).hashCode();
	// BUG: Diagnostic contains: h = Float.hashCode(dd.floatValue());
	h = new Float(dd).hashCode();
	// BUG: Diagnostic contains: return Long.hashCode(z);
	return new Long(z).hashCode();
    }
}

//  public void hashCodeInJava7()
abstract class Test9 {
    abstract int g(Integer x);
    int f(long z) {
	// BUG: Diagnostic contains: return Longs.hashCode(z);
	return new Long(z).hashCode();
    }
}

//  public void autoboxWidening()
class Test10 {
    void f(float f) {
	// BUG: Diagnostic contains: (double) f;
	Double d = new Double(f);
	// BUG: Diagnostic contains: (short) (byte) 0;
	Short s = new Short((byte) 0);
    }
}

//  public void autoboxGenerics()
class Test11 {
    <T> T f(Object o) {
	// BUG: Diagnostic contains: return (T) Integer.valueOf(o.hashCode());
	return (T) new Integer(o.hashCode());
    }
}
