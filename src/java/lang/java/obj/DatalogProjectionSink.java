package lang.java.obj;

import lang.relation.TupleInserter;

public class DatalogProjectionSink {
	public TupleInserter ast = TupleInserter.NULL;
	public TupleInserter provenance = TupleInserter.NULL;
	public TupleInserter attributes = TupleInserter.NULL;

	public DatalogProjectionSink(TupleInserter ast, TupleInserter provenance, TupleInserter attributes) {
		if (ast != null)
			this.ast = ast;
		if (provenance != null)
			this.provenance = provenance;
		if (attributes != null)
			this.attributes = attributes;
	}
}
