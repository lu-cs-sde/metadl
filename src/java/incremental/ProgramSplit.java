package incremental;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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
import lang.ast.ProgramRepresentation;
import lang.ast.Rule;
import lang.ast.StringType;
import lang.ast.Term;
import lang.relation.RelationWrapper;

import static lang.ast.Constructors.*;

public class ProgramSplit {
	private Program program;
	private Program localProgram;
	private Program globalProgram;
	private Program updateProgram;
	private Set<String> localOutputs = new TreeSet<>();

	public ProgramSplit(Program program) {
		this.program = program;
		split();
		generateUpdateProgram();
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
		return implicitTypeDeclaration(p.getPRED_ID(), p.type());
	}

	private static Rule implicitTypeDeclaration(String predName, PredicateType t) {
		Term[] terms = new Term[t.arity()];
		for (int i = 0; i < t.arity(); ++i) {
			if (t.get(i) == IntegerType.get() ||
				t.get(i) == ASTNodeType.get()) {
				terms[i] = integer(0);
			} else if (t.get(i) == StringType.get()) {
				terms[i] = str("");
			} else {
				assert t.get(i) == PredicateRefType.get();
				terms[i] = new PredicateRef(predName);
			}
		}

		return rule(literal(predName, (Object[]) terms), NEQ(integer(0), integer(0)));
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

		fpEDB.eval(program.evalCtx());

		RelationWrapper EDBs = new RelationWrapper(program.evalCtx(), fpEDB.relation2(), fpEDB.type());
		for (RelationWrapper.TupleWrapper t : EDBs.tuples()) {
			String pred = t.getAsString(0);
			String file = t.getAsString(1);
			String format = t.getAsString(2);

			FormalPredicate p = program.formalPredicateMap().get(pred);
			if (p.hasLocalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(pred), str(file), str(format))));
			}
			if (p.hasGlobalUse()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(pred), str(file), str(format))));
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

		fpOUTPUT.eval(program.evalCtx());

		RelationWrapper OUTPUTs = new RelationWrapper(program.evalCtx(), fpOUTPUT.relation2(), fpOUTPUT.type());
		for (RelationWrapper.TupleWrapper t : OUTPUTs.tuples()) {
			String pred = t.getAsString(0);
			String file = t.getAsString(1);
			// String format = t.getAsString(2);
			// TODO: force the format to sqlite, the tests expect this
			String format = "sqlite";

			FormalPredicate p = program.formalPredicateMap().get(pred);
			if (p.hasLocalDef()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(pred), str(file), str(format))));
				localOutputs.add(pred);
			}
			if (p.hasGlobalDef()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(pred), str(file), str(format))));
			}
		}
	}

	private void split() {
		localProgram = new Program();
		globalProgram = new Program();

		splitEDB();
		splitOUTPUT();

		for (FormalPredicate p : program.getFormalPredicates()) {
			if (p.getProgramRepresentationKind().isPresent() &&
				p.getProgramRepresentationKind().get() == ProgramRepresentation.ATTR_PROVENANCE) {
				// The provenance relation must be globaly available. It is used to decide which
				// files need an update.
				localOutputs.add(p.getPRED_ID());
			}

			if (p.hasLocalDef() && p.hasGlobalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(p.getPRED_ID()), str("ignored"), str("sqlite"))));
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str("ignored"), str("sqlite"))));
				localOutputs.add(p.getPRED_ID());
			}

			if (p.hasLocalUse() && p.getProgramRepresentationKind().isPresent()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str("ignored"), str("sqlite"))));
			}

			if (p.hasGlobalUse() && p.getProgramRepresentationKind().isPresent()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str("ignored"), str("sqlite"))));
				localOutputs.add(p.getPRED_ID());
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

	public static String AST_VISIT_RELATION = "AST_VISIT";
	public static String AST_REMOVE_RELATION = "AST_REMOVE";
	public static String ANALYZED_SOURCES_RELATION = "ANALYZED_SOURCES";

	private void generateUpdateClauses(Program p, AnalyzeBlock b) {
		String ATTR_PROVENANCE = b.getContext().provenanceRelName;
		String SRC_LOC = b.getContext().srcRelName;

		// Type declarations
		p.addCommonClause(implicitTypeDeclaration(ANALYZED_SOURCES_RELATION, new PredicateType(StringType.get(),
																			   StringType.get())));
		p.addCommonClause(implicitTypeDeclaration(ATTR_PROVENANCE,
												  FormalPredicate.programRepresentationType(ProgramRepresentation.ATTR_PROVENANCE)));
		p.addCommonClause(implicitTypeDeclaration(SRC_LOC,
												  FormalPredicate.programRepresentationType(ProgramRepresentation.SRC)));

		// AST_VISIT_RELATION - the files that need to be revisited
		// visit the files that were modified
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("M"))));
		// visit new files
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("A"))));
		// visit the files where attributes affected by a file change are computed
		// TODO: this can be refined, to only compute the attributes, not traverse the entire file
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f_attr"), "_"),
							   literal(ATTR_PROVENANCE, var("n"), "_", var("f_attr")),
							   literal(SRC_LOC, var("n"), "_", "_", "_", "_", var("f"))));
		// AST_REMOVE_RELATION - files to remove
		p.addCommonClause(rule(literal(AST_REMOVE_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("D"))));

		// Input
		// The ANALYZED_SOURCES_RELATION predicate relation should be filled in by the caller
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(ATTR_PROVENANCE), str("internal"), str("sqlite")),
							   literal(GlobalNames.EDB_NAME, ref(SRC_LOC), str("internal"), str("sqlite"))));

		// Output
		// p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_VISIT_RELATION)),
		// 					   literal(GlobalNames.OUTPUT_NAME, ref(AST_REMOVE_RELATION))));
	}

	private void generateUpdateProgram() {
		updateProgram = new Program();
		generateUpdateClauses(updateProgram, program.analyzeBlocks().get(0));
	}

	public Program getGlobalProgram() {
		return globalProgram;
	}

	public Program getUpdateProgram() {
		return updateProgram;
	}

	public Program getLocalProgram() {
		return localProgram;
	}

	public Program getProgram() {
		return program;
	}

	public Set<String> getLocalOutputs() {
		return Collections.unmodifiableSet(localOutputs);
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

	public boolean canEvaluateIncrementally() {
		if (program.analyzeBlocks().isEmpty()) {
			// the program must have at least one analyze block
			return false;
		}

		if (program.analyzeBlocks().stream()
			.map(b -> b.getProgramRef().getPRED_ID()).distinct().count() != 1) {
			// all analyze blocks must use the same program relation
			return false;
		}

		if (program.analyzeBlocks().stream()
			.anyMatch(b -> !b.getLang().getSTRING().equals("java8"))) {
			// all analyze blocks must use the Java language
			return false;
		}
		return true;
	}

	public FormalPredicate getSourceRelation() {
		return program.formalPredicateMap().get(program.analyzeBlocks().get(0).getProgramRef().getPRED_ID());
	}
}
