package incremental;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.HashSet;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;
import lang.ast.ASTNodeType;
import lang.ast.AnalyzeContext;
import lang.ast.Clause;
import lang.ast.CommonClause;
import lang.ast.CommonLiteral;
import lang.ast.Constraint;
import lang.ast.Fact;
import lang.ast.FormalPredicate;
import lang.ast.Functor;
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
import lang.ast.Variable;
import lang.ast.Expr;
import eval.EvaluationContext;
import lang.relation.RelationWrapper;

import static lang.ast.Constructors.*;

public class ProgramSplit {
	private final Program program;
	private Program updateProgram;
	private Program fusedProgram;
	private final Set<String> cachedPredicates = new TreeSet<>();

	public ProgramSplit(final Program program) {
		this.program = program;
		generateUpdateProgram();
		generateFusedProgram();
	}

	private static Rule implicitTypeDeclaration(final FormalPredicate p) {
		return implicitTypeDeclaration(p.getPRED_ID(), p.type());
	}

	private static Rule implicitTypeDeclaration(final String predName, final PredicateType t) {
		final CommonLiteral[] literals = new CommonLiteral[t.arity() + 2];
		final Term[] vars = new Variable[t.arity()];

		for (int i = 0; i < t.arity(); ++i) {
			vars[i] = var("v_" + i);
			Expr rhs;
			if (t.get(i) == IntegerType.get()) {
				rhs = integer(0);
			} else if (t.get(i) == ASTNodeType.get()) {
				rhs = functor("id_to_node", integer(0));
			} else if (t.get(i) == StringType.get()) {
				rhs = str("");
			} else {
				assert t.get(i) == PredicateRefType.get();
				rhs = new PredicateRef(predName);
			}
			literals[i + 2] = BIND(var("v_" + i), rhs);
		}

		literals[0] = literal(predName, (Object[]) vars);
		literals[1] = NEQ(integer(0), integer(0));

		return rule(literals);
	}

	static class PredicateIOInfo {
		FormalPredicate p;
		String format;
		String fileName;

		static PredicateIOInfo of(final FormalPredicate p, final String fileName, final String format) {
			final PredicateIOInfo ret = new PredicateIOInfo();
			ret.p = p;
			ret.format = format;
			ret.fileName = fileName;
			return ret;
		}
	}

	/**
	   Compute all the EDB or OUTPUT predicates of the original program
	 */
	private List<PredicateIOInfo> getIOPredicateInfo(final String predName) {
		assert predName == GlobalNames.EDB_NAME || predName == GlobalNames.OUTPUT_NAME;

		final FormalPredicate fp = program.formalPredicateMap().get(predName);

		if (fp == null) {
			// no EDB in the program, nothing to do
			return Collections.emptyList();
		}

		fp.eval(program.evalCtx());

		final List<PredicateIOInfo> predInfo = new ArrayList<>();
		final EvaluationContext ctx = program.evalCtx();
		final RelationWrapper EDBs = new RelationWrapper(ctx, ctx.getRelation(fp), fp.type());
		for (final RelationWrapper.TupleWrapper t : EDBs.tuples()) {
			final String pred = t.getAsString(0);
			final String file = t.getAsString(1);
			final String format = t.getAsString(2);

			final FormalPredicate p = program.formalPredicateMap().get(pred);
			predInfo.add(PredicateIOInfo.of(p, file, format));
		}
		return predInfo;
	}

	static private Literal copyLiteralWithTag(Literal l, String tagVarName) {
		List <Term> terms = new ArrayList<>();
		for (Term t : l.getTermss()) {
			terms.add(t.treeCopy());
		}
		terms.add(new Variable(tagVarName));
		Literal r = literal(l.getPredicate().getPRED_ID() + "_local", terms.toArray());
		if (l.hasNot())
			return NOT(r);
		else
			return r;
	}

	private static String selectTagVariable(Clause c) {
		Set<String> localVars = c.localVariables();
		return localVars.stream().min((s1, s2) -> s1.compareTo(s2)).get();
	}

	private static boolean isTaggedPredicate(FormalPredicate p, Set<FormalPredicate> outputs) {
		return p.isASTPredicate() && (p.hasGlobalUse() || outputs.contains(p));
	}

	private static Clause transformLocalClause(Clause c, Set<FormalPredicate> outputs) {
		Clause ret = c instanceof Rule ? new Rule() : new Fact();

		String tagVariable = "_tag";

		boolean usedTag = false;
		boolean definedTag = false;

		for (CommonLiteral cl : c.getHeadss()) {
			Literal l = cl.asLiteral();
			if (isTaggedPredicate(l.getPredicate().formalpredicate(), outputs)) {
				// global predicate defined in a local clause
				assert c instanceof Rule;
				ret.addHeads(copyLiteralWithTag(l, tagVariable));
				usedTag = true;
			} else {
				ret.addHeads(l.treeCopy());
			}
		}

		if (c instanceof Rule) {
			Rule r = (Rule) c;

			for (CommonLiteral cl : r.getBodys()) {
				Literal l = cl.asLiteral();
				if (l == null) {
					assert cl instanceof Constraint;
					((Rule) ret).addBody(cl.treeCopy());
				} else if (isTaggedPredicate(l.getPredicate().formalpredicate(), outputs)) {
					usedTag = true;
					definedTag = true;
					((Rule) ret).addBody(copyLiteralWithTag(l, tagVariable));
				} else {
					((Rule) ret).addBody(l.treeCopy());
				}
			}

			if (usedTag && !definedTag) {
				((Rule) ret).addBody(BIND(var(tagVariable), extractFileIdFromASTNodeId(var(selectTagVariable(c)))));
			}
		}

		return ret;
	}

	private static CommonLiteral transformLiteral(CommonLiteral l) {
		if (l instanceof Pattern) {
			return ((Pattern) l).getLiteral().treeCopy();
		}
		if (l instanceof Literal) {
			final Literal ll = (Literal) l;
			final Literal c = ll.treeCopy();
			return c;
		}
		return l.treeCopy();
	}

	private static Clause transformClause(Clause c) {
				final lang.ast.List<CommonLiteral> head = new lang.ast.List<>();

		for (final CommonLiteral l : c.getHeadss()) {
			head.add(transformLiteral(l));
		}

		if (c instanceof Rule) {
			final lang.ast.List<CommonLiteral> body = new lang.ast.List<>();
			final Rule r = (Rule) c;
			for (CommonLiteral l : r.getBodys()) {
				body.add(transformLiteral(l));
			}
			return new Rule(head, body);
		} else {
			return new Fact(head);
		}

	}

	private void generateFusedProgram() {
		fusedProgram = new Program();
		final List<PredicateIOInfo> outputs = getIOPredicateInfo(GlobalNames.OUTPUT_NAME);
		final Set<FormalPredicate> outputPreds = outputs.stream().map(e -> e.p).collect(Collectors.toSet());

		for (final CommonClause c : program.getCommonClauses()) {
			final Clause d = (Clause) c;
			if (d.isLocal() && d.isASTClause()) {
				fusedProgram.addCommonClause(transformLocalClause(d, outputPreds));
			} else {
				fusedProgram.addCommonClause(transformClause(d));
			}
		}

		for (final Clause d : program.getExpandedClauses()) {
			if (d.isLocal() && d.isASTClause()) {
				fusedProgram.addCommonClause(transformLocalClause(d, outputPreds));
			} else {
				fusedProgram.addCommonClause(transformClause(d));
			}
		}

		for (FormalPredicate p : program.getFormalPredicates()) {
			if (p.hasLocalDef() && isTaggedPredicate(p, outputPreds)) {
				// P(x) :- P_local(x, _tag).
				fusedProgram.addCommonClause(rule(literal(p.getPRED_ID(),
														  IntStream.range(0, p.realArity()).mapToObj(i -> var("v_" + i)).toArray()),
												  literal(p.getPRED_ID() + "_local",
														  IntStream.range(0, p.realArity() + 1).mapToObj(i -> var("v_" + i)).toArray())));
				// P(x) :- P_cache(x, _tag).
				fusedProgram.addCommonClause(rule(literal(p.getPRED_ID(),
														  IntStream.range(0, p.realArity()).mapToObj(i -> var("v_" + i)).toArray()),
												  literal(p.getPRED_ID() + "_cache",
														  IntStream.range(0, p.realArity() + 1).mapToObj(i -> var("v_" + i)).toArray())));

				PredicateType predType = p.type();
				PredicateType cachePredType = new PredicateType(predType.arity() + 1);
				for (int i = 0; i < predType.arity(); ++i) {
					cachePredType.joinAt(i, predType.get(i));
				}
				cachePredType.joinAt(predType.arity(), IntegerType.get());

				// Implicit type declaration for P_cache. P_local does not need an implicit type declaration
				// since the compiler should always be able to deduce it.
				fusedProgram.addCommonClause(implicitTypeDeclaration(p.getPRED_ID() + "_cache", cachePredType));


				// Output P_local to the cache
				fusedProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(p.getPRED_ID() + "_local"),
														  str(makeInternalDbDesc(p.getPRED_ID(), "append")),
														  str("sqlite"))));

				// Read in P_cache from cache
				fusedProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(p.getPRED_ID() + "_cache"),
														  str(makeInternalDbDesc(p.getPRED_ID(), "read")),
														  str("sqlite"))));

				cachedPredicates.add(p.getPRED_ID());
			}
		}

		// Add all the program representation predicates as dummy inputs. This prevents
		// Souffle to remove them as dead code.
		for (ProgramRepresentation pr : ProgramRepresentation.values()) {
			String predName = program.getAnalysisContext().prefix(pr.getPredicateName());
			if (program.formalPredicateMap().containsKey(predName)) {
				fusedProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(predName), "__dummy__", "csv")));
			}
		}

		// cache the provenance relation
		String provPredName = program.getAnalysisContext().provenanceRelName;
		if (!cachedPredicates.contains(provPredName)) {
			fusedProgram.addCommonClause(implicitTypeDeclaration(provPredName,
																 FormalPredicate.programRepresentationType(ProgramRepresentation.ATTR_PROVENANCE)));
			fusedProgram.addCommonClause(rule(literal(provPredName + "_local", var("file_id"), var("other_file_id"), var("file_id")),
											  literal(provPredName, var("file_id"), var("other_file_id"))));

			fusedProgram.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(provPredName + "_local"),
													  str(makeInternalDbDesc(provPredName, "append")),
													  str("sqlite"))));
			fusedProgram.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(provPredName), "__dummy__", "csv")));

			cachedPredicates.add(provPredName);
		}

	}

	public final static String AST_VISIT_RELATION = "AST_VISIT";
	public final static String AST_REMOVE_RELATION = "AST_REMOVE";
	public final static String ANALYZED_SOURCES_RELATION = "ANALYZED_SOURCES";
	public final static String FILE_ID = "FILE_ID";

	public static PredicateType getTypeForUpdateRelation(final String name) {
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

	public static String makeInternalDbDesc(String table, String mode) {
		return INTERNAL_DB_NAME + ":" + table + ":" + mode;
	}

	public static boolean isInternalDbDesc(String dbname) {
		return dbname.startsWith(INTERNAL_DB_NAME);
	}

	public static String getInternalDbTable(String desc) {
		String[] parts = desc.split(":");
		if (parts.length != 3)
			return null;
		return parts[1];
	}

	public static String getInternalDbMode(String desc) {
		String[] parts = desc.split(":");
		if (parts.length != 3)
			return "default";
		return parts[2];
	}

	public static Pair<String, String> parseInternalDbName(String dbname) {
		String[] parts = dbname.split(":");
		if (parts.length != 3)
			return Pair.of(null, "default");
		return Pair.of(parts[1], parts[2]);
	}

	private static Functor extractFileIdFromASTNodeId(Variable n) {
		return functor("band", functor("bshru", functor("node_to_id", n), integer(32)), integer((1 << 30) - 1));
	}

	private void generateUpdateClauses(final Program p) {
		final String ATTR_PROVENANCE = program.getAnalysisContext().provenanceRelName;

		// read the analyzed sources
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(ANALYZED_SOURCES_RELATION),
									   str(makeInternalDbDesc(ANALYZED_SOURCES_RELATION, "read")), str("sqlite"))));
		// read the file Id database
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(FILE_ID), str(makeInternalDbDesc(FILE_ID, "read")), str("sqlite"))));

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
		p.addCommonClause(rule(literal(AST_VISIT_RELATION, var("f")), literal(ANALYZED_SOURCES_RELATION, var("f_attr"), "_"),
							   literal(ATTR_PROVENANCE, var("fid"), var("fid_attr")),
							   literal(FILE_ID, var("f"), var("fid")),
							   literal(FILE_ID, var("f_attr"), var("fid_attr")),
							   NOT(literal(AST_REMOVE_RELATION, var("f")))));

		// Input
		// The ANALYZED_SOURCES_RELATION predicate relation should be filled in by the caller
		p.addCommonClause(fact(literal(GlobalNames.EDB_NAME, ref(ATTR_PROVENANCE),
									   makeInternalDbDesc(ATTR_PROVENANCE, "read"),
									   str("sqlite"))));

		// Output
		p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_VISIT_RELATION),
									   makeInternalDbDesc(AST_VISIT_RELATION, "append"), str("sqlite"))));
		p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_REMOVE_RELATION),
									   makeInternalDbDesc(AST_REMOVE_RELATION, "append"), str("sqlite"))));

		// Output (Enable for debug purposes)
		if (false) {
			p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(AST_VISIT_RELATION), str("AST_VISIT"), str("csv"))));
			p.addCommonClause(fact(literal(GlobalNames.OUTPUT_NAME, ref(ATTR_PROVENANCE), str("ATTR_PROV"), str("csv"))));
		}
	}

	private void generateUpdateProgram() {
		updateProgram = new Program();
		generateUpdateClauses(updateProgram);
	}

	public Program getUpdateProgram() {
		return updateProgram;
	}

	public Program getProgram() {
		return program;
	}

	public Program getFusedProgram() {
		return fusedProgram;
	}

	public Set<String> getCachedPredicates() {
		return Collections.unmodifiableSet(cachedPredicates);
	}

	public static class AnalysisInfo {
		public AnalyzeContext ctx;
		public String srcRelName;

		public AnalysisInfo(final AnalyzeContext ctx, final String srcRelName) {
			this.ctx = ctx;
			this.srcRelName = srcRelName;
		}
	}

	public boolean canEvaluateIncrementally() {
		return true;
	}
}
