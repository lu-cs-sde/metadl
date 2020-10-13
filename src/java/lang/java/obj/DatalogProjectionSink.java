package lang.java.obj;

import java.util.Map;

import eval.EvaluationContext;
import lang.ast.FormalPredicate;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;

public class DatalogProjectionSink {
	private TupleInserter ast;
	private TupleInserter provenance;
	private TupleInserter attributes;

	private FormalPredicate astPredicate;
	private FormalPredicate provenancePredicate;
	private FormalPredicate attributesPredicate;

	public DatalogProjectionSink(EvaluationContext ctx,
								 FormalPredicate astPredicate,
								 FormalPredicate provenancePredicate,
								 FormalPredicate attributesPredicate) {
		this.astPredicate = astPredicate;
		this.provenancePredicate = provenancePredicate;
		this.attributesPredicate = attributesPredicate;

		if (astPredicate != null) {
			ast = new RelationWrapper(ctx, astPredicate.relation2(), astPredicate.type());
		} else {
			ast = TupleInserter.NULL;
		}

		if (provenancePredicate != null) {
			provenance = new RelationWrapper(ctx, provenancePredicate.relation2(), provenancePredicate.type());
		} else {
			provenance = TupleInserter.NULL;
		}

		if (attributesPredicate != null) {
			attributes = new RelationWrapper(ctx, attributesPredicate.relation2(), attributesPredicate.type());
		} else {
			attributes = TupleInserter.NULL;
		}
	}

	private DatalogProjectionSink() { }

	public DatalogProjectionSink remap(Map<FormalPredicate, TupleInserter> sinkRemap) {
		DatalogProjectionSink res = new DatalogProjectionSink();
		res.astPredicate = astPredicate;
		res.ast = sinkRemap.getOrDefault(astPredicate, ast);
		res.provenancePredicate = provenancePredicate;
		res.provenance = sinkRemap.getOrDefault(provenancePredicate, provenance);
		res.attributesPredicate = attributesPredicate;
		res.attributes = sinkRemap.getOrDefault(attributesPredicate, attributes);
		return res;
	}

	public TupleInserter getAst() {
		return ast;
	}

	public TupleInserter getProvenance() {
		return provenance;
	}

	public TupleInserter getAttributes() {
		return attributes;
	}

}
