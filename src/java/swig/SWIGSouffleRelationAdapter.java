package swig;

import java.nio.ByteBuffer;

import lang.relation.TupleInserter;

public class SWIGSouffleRelationAdapter implements TupleInserter {
	private final static int BUFFER_SIZE = 100000;
	private long[] l1Buffer = new long[BUFFER_SIZE];
	private long[] l2Buffer = new long[BUFFER_SIZE];
	private long[] l3Buffer = new long[BUFFER_SIZE];
	private String[] s0Buffer = new String[BUFFER_SIZE];
	private String[] s4Buffer = new String[BUFFER_SIZE];
	int count = 0;
	int total = 0;

	SWIGSouffleRelation rel;
	public SWIGSouffleRelationAdapter(SWIGSouffleRelation rel) {
		this.rel = rel;
	}
	@Override
	public void insertTuple(Object... elems) {
		assert false;
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

	@Override
	public void insertTuple(String s0, long l1, long l2, long l3, String s4) {
		if (count == BUFFER_SIZE) {
			// rel.add(s0Buffer, l1Buffer, l2Buffer, l3Buffer, s4Buffer, count);
			count = 0;
		}

		s0Buffer[count] = s0;
		l1Buffer[count] = l1;
		l2Buffer[count] = l2;
		l3Buffer[count] = l3;
		s4Buffer[count] = s4;
		count++;
		total++;
	}

	@Override
	public void done() {
		if (count != 0) {
			// rel.add(s0Buffer, l1Buffer, l2Buffer, l3Buffer, s4Buffer, count);
			count = 0;
		}

		System.err.println("==== Added " + total + " tuples.");
	}
}
