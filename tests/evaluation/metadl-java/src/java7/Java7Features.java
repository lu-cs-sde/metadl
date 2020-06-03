import java.util.HashMap;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.*;


class Java7Features {
    HashMap<String, String> h = new HashMap<>();
    int i = 1_000_013;

    void tryWithResources() {
	try (FileOutputStream os = new FileOutputStream("bla.out");
	     FileInputStream is = new FileInputStream("bla.in")) {
	    int b = is.read();
	    os.write(b);
	} catch (IOException e) {
	    System.out.println(e);
	}
    }

    void multicatch() {
	try {
	    int[] x = null;
	    x[3] = 10;
	} catch (ArrayIndexOutOfBoundsException | NullPointerException e) {
	    System.out.println(e);
	}
    }
}
