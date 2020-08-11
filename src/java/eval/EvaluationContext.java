package eval;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EvaluationContext {
	private long stringID = 0;

	private Map<String, Long> stringMap = new HashMap<>();
	private List<String> longMap = new ArrayList<>();

	public long internalizeString(String s) {
		Long id = stringMap.get(s);
		if (id == null) {
			id = stringID++;
			stringMap.put(s, id);
			longMap.add(s);
		}
		return id;
	}

	public String externalizeString(long l) {
		if (l >= longMap.size())
			throw new RuntimeException("String not internalized: " + l);
		String s = longMap.get((int)l);
		return s;
	}
}
