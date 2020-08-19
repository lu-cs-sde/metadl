package lang.io;
import java.util.HashMap;
import java.util.Map;

public class StringUID {
	private static StringUID instance = null;
	public static synchronized StringUID getInstance() {
		if (instance == null)
			instance = new StringUID();
		return instance;
	}

	private int counter = Integer.MAX_VALUE;
	private Map<String, Integer> uids = new HashMap<>();
	public synchronized int uid(String s) {
		// Integer u = uids.get(s);
		Integer u = null;
		if (u == null) {
			uids.put(s, counter);
			return counter--;
		}
		return u;
	}
}
