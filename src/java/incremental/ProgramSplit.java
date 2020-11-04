package incremental;

import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import eval.EvaluationContext;
import lang.ast.ASTNodeType;
import lang.ast.AnalyzeBlock;
import lang.ast.AnalyzeContext;
import lang.ast.Clause;
import lang.ast.CommonClause;
import lang.ast.CommonLiteral;
import lang.ast.Fact;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.IntegerType;
import lang.ast.Literal;
import lang.ast.Pattern;
import lang.ast.PredicateRef;
import lang.ast.PredicateRefType;
import lang.ast.PredicateSymbol;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.Rule;
import lang.ast.StringType;
import lang.ast.Term;
import lang.relation.RelationWrapper;

import static lang.ast.Constructors.*;

public class ProgramSplit {
	private Program program;
	private Program computeChangedFilesProgram;
	private Program removeObsoleteFactsProgram;
	private Program localProgram;
	private Program globalProgram;

	public ProgramSplit(Program program) {
		this.program = program;
		split();
	}

	private static CommonLiteral copyLiteral(CommonLiteral l) {
		if (l instanceof Pattern) {
			return ((Pattern) l).getLiteral().treeCopy();
		}
		if (l instanceof Literal) {
			Literal ll = (Literal) l;
			Literal c = ll.treeCopy();
			if (ll.isOUTPUT()) {
				c.setPredicate(new PredicateSymbol("OUTPUT_"));
			} else if (ll.isEDB()) {
				c.setPredicate(new PredicateSymbol("EDB_"));
			}
			return c;
		}
		return l.treeCopy();
	}

	private static Clause copyClause(Clause c) {
		lang.ast.List<CommonLiteral> head = new lang.ast.List<>();


		for (CommonLiteral l : c.getHeadss()) {
			head.add(copyLiteral(l));
		}

		if (c instanceof Rule) {
			lang.ast.List<CommonLiteral> body = new lang.ast.List<>();
			Rule r = (Rule) c;
			for (CommonLiteral l : r.getBodys()) {
				body.add(copyLiteral(l));
			}
			return new Rule(head, body);
		} else {
			return new Fact(head);
		}
	}

	private static Rule implicitTypeDeclaration(FormalPredicate p) {
		PredicateType t = p.type();
		Term[] terms = new Term[t.arity()];
		for (int i = 0; i < t.arity(); ++i) {
			if (t.get(i) == IntegerType.get() ||
				t.get(i) == ASTNodeType.get()) {
				terms[i] = integer(0);
			} else if (t.get(i) == StringType.get()) {
				terms[i] = str("");
			} else {
				assert t.get(i) == PredicateRefType.get();
				terms[i] = new PredicateRef(p.getPRED_ID());
			}
		}

		return rule(literal(p.getPRED_ID(), (Object[]) terms), NEQ(integer(0), integer(0)));
	}

	/**
	   Compute all the EDB predicates of the original program and emit the
	   new EDB facts in the corresponding partition.
	 */
	private void splitEDB() {
		FormalPredicate fpEDB = program.formalPredicateMap().get(GlobalNames.EDB_NAME);

		if (fpEDB == null) {
			// no EDB in the program, nothing to do
			return;
		}

		EvaluationContext ctx = new EvaluationContext();
		fpEDB.eval(ctx);

		RelationWrapper EDBs = new RelationWrapper(ctx, fpEDB.relation2(), fpEDB.type());
		for (RelationWrapper.TupleWrapper t : EDBs.tuples()) {
			String pred = t.getAsString(0);
			String file = t.getAsString(1);

			FormalPredicate p = program.formalPredicateMap().get(pred);
			if (p.hasLocalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(pred), file)));
			}
			if (p.hasGlobalUse()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(pred), file)));
			}
		}
	}

	/**
	   Compute all the OUTPUT predicates of the original program and emit the
	   new OUTPUT facts in the corresponding partition.
	 */
	private void splitOUTPUT() {
		FormalPredicate fpOUTPUT = program.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);

		if (fpOUTPUT == null) {
			// no OUTPUT in the program, nothing to do
			return;
		}

		EvaluationContext ctx = new EvaluationContext();
		fpOUTPUT.eval(ctx);

		RelationWrapper OUTPUTs = new RelationWrapper(ctx, fpOUTPUT.relation2(), fpOUTPUT.type());
		for (RelationWrapper.TupleWrapper t : OUTPUTs.tuples()) {
			String pred = t.getAsString(0);

			FormalPredicate p = program.formalPredicateMap().get(pred);
			if (p.hasLocalDef()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(pred))));
			}
			if (p.hasGlobalDef()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(pred))));
			}
		}
	}

	private void split() {
		localProgram = new Program();
		globalProgram = new Program();

		splitEDB();
		splitOUTPUT();

		for (FormalPredicate p : program.getFormalPredicates()) {
			if (p.hasLocalDef() && p.hasGlobalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(p.getPRED_ID()))));
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str(p.getPRED_ID() + ".csv"))));
			}

			if (p.hasLocalUse() && p.getProgramRepresentationKind().isPresent()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str(p.getPRED_ID() + ".csv"))));
			}

			if (p.hasGlobalUse() || p.hasGlobalDef()) {
				globalProgram.addCommonClause(implicitTypeDeclaration(p));
			}

			if (p.hasLocalUse() || p.hasLocalDef()) {
				localProgram.addCommonClause(implicitTypeDeclaration(p));
			}
		}

		Consumer<Clause> clauseSeparator = (Clause c) -> {
			if (c.isLocal())
				localProgram.addCommonClause(copyClause(c));
			else
				globalProgram.addCommonClause(copyClause(c));
		};

		for (CommonClause c : program.getCommonClauses()) {
			if (c instanceof AnalyzeBlock) {
				for (Clause d : ((AnalyzeBlock) c).getExpandedClauses()) {
					clauseSeparator.accept(d);
				}
				for (Clause d : ((AnalyzeBlock) c).getClauses()) {
					clauseSeparator.accept(d);
				}
			} else {
				clauseSeparator.accept((Clause) c);
			}
		}
	}

	public Program getGlobalProgram() {
		return globalProgram;
	}

	public Program getLocalProgram() {
		return localProgram;
	}

	public static class AnalysisInfo {
		public AnalyzeContext ctx;
		public String srcRelName;

		public AnalysisInfo(AnalyzeContext ctx, String srcRelName) {
			this.ctx = ctx;
			this.srcRelName = srcRelName;
		}
	}


	public List<AnalysisInfo> getAnalyzeContexts() {
		return program.analyzeBlocks().stream().map(b -> new AnalysisInfo(b.getContext(), b.getProgramRef().getPRED_ID()))
													.collect(Collectors.toList());
	}
}
