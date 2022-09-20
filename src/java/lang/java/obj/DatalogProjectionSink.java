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
	private TupleInserter srcLoc;

	private FormalPredicate astPredicate;
	private FormalPredicate provenancePredicate;
	private FormalPredicate attributesPredicate;
	private FormalPredicate srcLocPredicate;

	public DatalogProjectionSink(EvaluationContext ctx,
								 FormalPredicate astPredicate,
								 FormalPredicate provenancePredicate,
								 FormalPredicate attributesPredicate,
								 FormalPredicate srcLocPredicate) {
		this.astPredicate = astPredicate;
		this.provenancePredicate = provenancePredicate;
		this.attributesPredicate = attributesPredicate;
		this.srcLocPredicate = srcLocPredicate;

		if (astPredicate != null) {
			ast = new RelationWrapper(ctx, ctx.getRelation(astPredicate), astPredicate.type());
		} else {
			ast = TupleInserter.NULL;
		}

		if (provenancePredicate != null) {
			provenance = new RelationWrapper(ctx, ctx.getRelation(provenancePredicate), provenancePredicate.type());
		} else {
			provenance = TupleInserter.NULL;
		}

		if (attributesPredicate != null) {
			attributes = new RelationWrapper(ctx, ctx.getRelation(attributesPredicate), attributesPredicate.type());
		} else {
			attributes = TupleInserter.NULL;
		}

		if (srcLocPredicate != null) {
			srcLoc = new RelationWrapper(ctx, ctx.getRelation(srcLocPredicate), srcLocPredicate.type());
		} else {
			srcLoc = TupleInserter.NULL;
		}
	}

	protected DatalogProjectionSink() { }

	public DatalogProjectionSink remap(Map<FormalPredicate, TupleInserter> sinkRemap) {
		DatalogProjectionSink res = new DatalogProjectionSink();
		res.astPredicate = astPredicate;
		res.ast = sinkRemap.getOrDefault(astPredicate, ast);

		res.provenancePredicate = provenancePredicate;
		res.provenance = sinkRemap.getOrDefault(provenancePredicate, provenance);

		res.attributesPredicate = attributesPredicate;
		res.attributes = sinkRemap.getOrDefault(attributesPredicate, attributes);

		res.srcLocPredicate = srcLocPredicate;
		res.srcLoc = sinkRemap.getOrDefault(srcLocPredicate, srcLoc);

		return res;
	}

	public TupleInserter getAST() {
		return ast;
	}

	public TupleInserter getProvenance() {
		return provenance;
	}

	public TupleInserter getAttributes() {
		return attributes;
	}

	public TupleInserter getSrcLoc() {
		return srcLoc;
	}
}
