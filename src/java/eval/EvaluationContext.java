package eval;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lang.ast.FormalPredicate;

public class EvaluationContext {
	// strings
	private long stringID = 0;

	private Map<String, Long> stringMap = new HashMap<>();
	private List<String> longMap = new ArrayList<>();

	public long internalizeString(String s) {
		if (s == null)
			throw new RuntimeException("Cannot interanlize null.");

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

	// relations
	private Map<String, Relation2> relationMap = new HashMap<>();
	private Map<String, Relation2> deltaMap = new HashMap<>();
	private Map<String, Relation2> nextMap = new HashMap<>();

	private static Relation2 getRelationInternal(String name, int arity, Map<String, Relation2> m) {
		Relation2 rel = m.get(name);
		if (rel == null) {
			rel = new Relation2(arity);
			m.put(name, rel);
		}
		return rel;
	}

	public Relation2 getRelation(String fpName, int arity) {
		return getRelationInternal(fpName, arity, relationMap);
	}

	public Relation2 getRelation(FormalPredicate fp) {
		return getRelation(fp.getPRED_ID(), fp.realArity());
	}

	public Relation2 getDeltaRelation(FormalPredicate fp) {
		return getRelationInternal(fp.getPRED_ID(), fp.realArity(), deltaMap);
	}

	public Relation2 getNextRelation(FormalPredicate fp) {
		return getRelationInternal(fp.getPRED_ID(), fp.realArity(), nextMap);
	}

	/**
	   Empty all the relations in this context. This is necessary when
	   a Program object is evaluated multiple times.
	 */
	public void resetRelations() {
		for (Relation2 r : relationMap.values()) {
			r.clear();
		}

		for (Relation2 r : deltaMap.values()) {
			r.clear();
		}

		for (Relation2 r : nextMap.values()) {
			r.clear();
		}
	}
}
