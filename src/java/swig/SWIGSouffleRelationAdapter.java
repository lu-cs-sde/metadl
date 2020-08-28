package swig;

import lang.relation.TupleInserter;

public class SWIGSouffleRelationAdapter implements TupleInserter {
	SWIGSouffleRelation rel;
	public SWIGSouffleRelationAdapter(SWIGSouffleRelation rel) {
		this.rel = rel;
	}
	@Override
	public void insertTuple(Object... elems) {
		SWIGSouffleTuple tpl = rel.makeTuple();
		for (Object elem : elems) {
			if (elem instanceof Long) {
				tpl.add((long) elem);
			} else if (elem instanceof String) {
				tpl.add((String) elem);
			} else if (elem instanceof Integer) {
				tpl.add((long) (int) elem);
			} else {
				throw new RuntimeException("Unknown type for tuple element.");
			}
		}
		rel.add(tpl);
	}

}
