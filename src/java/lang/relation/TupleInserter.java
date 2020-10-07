package lang.relation;

public interface TupleInserter {
	void insertTuple(final Object... elems);
	default void insertTuple(String s0, long l1, long l2, long l3, String s4) {
		insertTuple((Object)s0, l1, l2, l3, s4);
	}
	default void done() {};

	static TupleInserter NULL =
		new TupleInserter() {
			@Override public void insertTuple(final Object... elems) {
				// do nothing
			}
		};
}
