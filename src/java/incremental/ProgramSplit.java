package incremental;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.HashSet;
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
	private Set<String> cachedPredicates = new TreeSet<>();

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

	static class PredicateIOInfo {
		FormalPredicate p;
		String format;
		String fileName;

		static PredicateIOInfo of(FormalPredicate p, String fileName, String format) {
			PredicateIOInfo ret = new PredicateIOInfo();
			ret.p = p;
			ret.format = format;
			ret.fileName = fileName;
			return ret;
		}
	}

	/**
	   Compute all the EDB or OUTPUT predicates of the original program
	 */
	private List<PredicateIOInfo> getIOPredicateInfo(String predName) {
		assert predName == GlobalNames.EDB_NAME || predName == GlobalNames.OUTPUT_NAME;

		FormalPredicate fp = program.formalPredicateMap().get(predName);

		if (fp == null) {
			// no EDB in the program, nothing to do
			return Collections.emptyList();
		}

		fp.eval(program.evalCtx());

		List<PredicateIOInfo> predInfo = new ArrayList<>();
		RelationWrapper EDBs = new RelationWrapper(program.evalCtx(), fp.relation2(), fp.type());
		for (RelationWrapper.TupleWrapper t : EDBs.tuples()) {
			String pred = t.getAsString(0);
			String file = t.getAsString(1);
			String format = t.getAsString(2);

			FormalPredicate p = program.formalPredicateMap().get(pred);
			predInfo.add(PredicateIOInfo.of(p, file, format));
		}
		return predInfo;
	}

	private void split() {
		localProgram = new Program();
		globalProgram = new Program();

		List<PredicateIOInfo> edbs = getIOPredicateInfo(GlobalNames.EDB_NAME);
		for (PredicateIOInfo edb : edbs) {
			if (edb.p.hasLocalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, str(edb.p.getPRED_ID()), str(edb.fileName), str(edb.format))));
			}
			if (edb.p.hasGlobalUse()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, str(edb.p.getPRED_ID()), str(edb.fileName), str(edb.format))));
			}
		}

		List<PredicateIOInfo> outputs = getIOPredicateInfo(GlobalNames.OUTPUT_NAME);
		for (PredicateIOInfo output : outputs) {
			// outputs are always in the global program
			globalProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(output.p.getPRED_ID()), str(output.fileName), str(output.format))));
		}

		Set<FormalPredicate> outputPreds = outputs.stream().map(e -> e.p).collect(Collectors.toSet());

		for (FormalPredicate p : program.getFormalPredicates()) {
			if (p.hasLocalDef() && (p.hasGlobalUse() || outputPreds.contains(p))) {
				localProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(p.getPRED_ID()), str(INTERNAL_DB_NAME), str("sqlite"))));
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str(INTERNAL_DB_NAME), str("sqlite"))));
				cachedPredicates.add(p.getPRED_ID());
			}

			if (p.hasLocalUse() && p.getProgramRepresentationKind().isPresent()) {
				localProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str(INTERNAL_DB_NAME), str("sqlite"))));
				cachedPredicates.add(p.getPRED_ID());
			}

			if ((p.hasGlobalUse() || outputPreds.contains(p)) && p.getProgramRepresentationKind().isPresent()) {
				globalProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID()), str(INTERNAL_DB_NAME), str("sqlite"))));
				cachedPredicates.add(p.getPRED_ID());
			}

			if (p.hasGlobalUse() || p.hasGlobalDef() || outputPreds.contains(p)) {
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

	public final static String AST_VISIT_RELATION = "AST_VISIT";
	public final static String AST_REMOVE_RELATION = "AST_REMOVE";
	public final static String ANALYZED_SOURCES_RELATION = "ANALYZED_SOURCES";
	public final static String FILE_ID = "FILE_ID";
	public final static String EXTERNAL_FILE = "EXTERNAL_FILE";

	public static PredicateType getTypeForUpdateRelation(String name) {
		switch (name) {
		case AST_VISIT_RELATION:
		case AST_REMOVE_RELATION:
			return new PredicateType(StringType.get());
		case ANALYZED_SOURCES_RELATION:
			return new PredicateType(StringType.get(), StringType.get());
		case FILE_ID:
			return new PredicateType(StringType.get(), IntegerType.get());
		default:
			throw new RuntimeException("Unknown relation name " + name);
		}
	}

	public static String INTERNAL_DB_NAME = "__internal__";

	private void generateUpdateClauses(Program p, AnalyzeBlock b) {
		String ATTR_PROVENANCE = b.getContext().provenanceRelName;
		String SRC_LOC = b.getContext().srcRelName;

		cachedPredicates.add(ATTR_PROVENANCE);
		cachedPredicates.add(SRC_LOC);

		// read the analyzed sources
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(ANALYZED_SOURCES_RELATION), str(INTERNAL_DB_NAME), str("sqlite"))));
		// read the file Id database
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(FILE_ID), str(INTERNAL_DB_NAME), str("sqlite"))));

		// Type declarations
		p.addCommonClause(implicitTypeDeclaration(ANALYZED_SOURCES_RELATION, getTypeForUpdateRelation(ANALYZED_SOURCES_RELATION)));

		p.addCommonClause(implicitTypeDeclaration(FILE_ID, getTypeForUpdateRelation(FILE_ID)));

		p.addCommonClause(implicitTypeDeclaration(ATTR_PROVENANCE,
												  FormalPredicate.programRepresentationType(ProgramRepresentation.ATTR_PROVENANCE)));

		// AST_REMOVE_RELATION - files to remove
		p.addCommonClause(rule(literal(AST_REMOVE_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("D"))));

		// AST_VISIT_RELATION - the files that need to be revisited
		// visit the files that were modified
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("M"))));
		// visit new files
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f"), str("A"))));
		// visit the files where attributes affected by a file change are computed
		// TODO: this can be refined, to only compute the attributes, not traverse the entire file
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f_attr"), "_"),
							   literal(ATTR_PROVENANCE, var("n"), "_", var("f_attr")),
							   BIND(var("fid"), functor("band", functor("bshru", var("n"), integer(32)), integer((1 << 30) - 1))),
							   literal(FILE_ID, var("f"), var("fid")),
							   NOT(literal(AST_REMOVE_RELATION, var("f")))));

		// Input
		// The ANALYZED_SOURCES_RELATION predicate relation should be filled in by the caller
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(ATTR_PROVENANCE), str(INTERNAL_DB_NAME), str("sqlite"))));

		// Output
		p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_VISIT_RELATION), str(INTERNAL_DB_NAME), str("sqlite"))));
		p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_REMOVE_RELATION), str(INTERNAL_DB_NAME), str("sqlite"))));

		// Output (Enable for debug purposes)
		if (false) {
			p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_VISIT_RELATION), str("AST_VISIT"), str("csv"))));
			p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(ATTR_PROVENANCE), str("ATTR_PROV"), str("csv"))));
		}
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

	public Set<String> getCachedPredicates() {
		return Collections.unmodifiableSet(cachedPredicates);
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
