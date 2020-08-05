package lang.cons;

import lang.ast.*;
import lang.ast.AnalyzeContext;
import java.util.ArrayList;

public abstract class PatternConstraint {
	public Clause generateClause(AnalyzeContext ctx) {return null;};
	public abstract ArrayList<Literal> generateLiterals(AnalyzeContext ctx);
	protected ObjLangASTNode node;
	protected PatternConstraint(ObjLangASTNode node) {
		this.node = node;
	}

	protected static Atom makeAtom(String pred, Term... ts) {
		PredicateSymbol ps = new PredicateSymbol(pred);
		List<Term> terms = new List<>(ts);
		return new Atom(ps, terms);
	}

	protected static Atom makeListAtom(AnalyzeContext ctx,
									   Term nodeId,
									   Term childIdx,
									   Term childId) {
		PredicateSymbol ps = new PredicateSymbol(ctx.progRelName);
		Term list = new StringConstant("List");
		List<Term> terms = new List<>();
		terms.add(list).add(nodeId).add(childIdx).add(childId).add(new Wildcard());
		return new Atom(ps, terms);

	}

	protected Expr makeAdd(Expr left, Expr right) {
		List<Expr> args = new List<>();
		args.add(left);
		args.add(right);
		return new Functor(new FunctorSymbol("add"), args);
	}

	public static PatternConstraint member(ObjLangASTNode n) {
		return new MemberConstraint(n);
	}

	public static PatternConstraint eq(ObjLangASTNode n1, int pos) {
		return new EqualityConstraint(n1, pos);
	}

	public static PatternConstraint last(ObjLangASTNode n) {
		return new LastConstraint(n);
	}

	public static PatternConstraint after(ObjLangASTNode n, ObjLangASTNode after) {
		return new AfterConstraint(after, n);
	}

	public static PatternConstraint next(ObjLangASTNode n, ObjLangASTNode next) {
		return new NextConstraint(next, n);
	}
}

class EqualityConstraint extends PatternConstraint {
	private int pos;
	public EqualityConstraint(ObjLangASTNode node, int pos) {
		super(node);
		this.pos = pos;
	}
	public ArrayList<Literal> generateLiterals(AnalyzeContext ctx) {
		ArrayList<Literal> result = new ArrayList<>();
		result.add(makeListAtom(ctx,
								new Variable(node.getParent().varName()),
								new Variable(node.indexVarName()),
								new Variable(node.varName())));

		result.add(new EQLiteral(new PredicateSymbol("EQ"),
								 new Variable(node.indexVarName()),
								 new IntConstant("" + pos)));
		return result;
	}
}

class MemberConstraint extends PatternConstraint {
	ArrayList<Literal> result = new ArrayList<>();
	final String boundVarName = "b_" + node.getNodeId();
	public MemberConstraint(ObjLangASTNode node) {
		super(node);
	}
	public ArrayList<Literal> generateLiterals(AnalyzeContext ctx) {
		ArrayList<Literal> result = new ArrayList<>();
		result.add(makeListAtom(ctx,
								new Variable(node.getParent().varName()),
								new Variable(node.indexVarName()),
								new Variable(node.varName())));
		return result;
	}
}

class LastConstraint extends PatternConstraint {
	final String newRuleName = node.getParent().getRelation();
	// This element must be the last one in the list.
	// Check that the relation List(l, _, i+1) is empty.
	// List1(l, i) :- List(l, i, c).
	// List(l, i, child), BIND(j, i+1), NEG(List1(l, j)).
	public LastConstraint(ObjLangASTNode node) {
		super(node);
	}

	public ArrayList<Literal> generateLiterals(AnalyzeContext ctx) {
		ArrayList<Literal> result = new ArrayList<>();
		String boundVarName = "b_" + node.getNodeId();
		result.add(makeListAtom(ctx,
								new Variable(node.getParent().varName()),
								new Variable(node.indexVarName()),
								new Variable(node.varName())));
		result.add(new BINDLiteral(new PredicateSymbol("BIND"),
								   new Variable(boundVarName),
								   makeAdd(new Variable(node.indexVarName()),
										   new IntConstant("1"))));
		result.add(new NEGLiteral(new PredicateSymbol("NEG"),
								  makeListAtom(ctx,
											   new Variable(node.getParent().varName()),
											   new Variable(boundVarName),
											   new Wildcard())));
		return result;
	}
}

class AfterConstraint extends PatternConstraint {
	private ObjLangASTNode predecessor;

	public AfterConstraint(ObjLangASTNode node, ObjLangASTNode predecessor) {
		super(node);
		this.predecessor = predecessor;
	}

	public ArrayList<Literal> generateLiterals(AnalyzeContext ctx) {
		ArrayList<Literal> result = new ArrayList<>();
		result.add(makeListAtom(ctx,
								new Variable(node.getParent().varName()),
								new Variable(node.indexVarName()),
								new Variable(node.varName())));
		result.add(new GTLiteral(new PredicateSymbol("GT"),
								 new Variable(node.indexVarName()),
								 new Variable(predecessor.indexVarName())));
		return result;
	}
}

class NextConstraint extends PatternConstraint {
	private ObjLangASTNode predecessor;
	public NextConstraint(ObjLangASTNode node, ObjLangASTNode predecessor) {
		super(node);
		this.predecessor = predecessor;
	}

	public ArrayList<Literal> generateLiterals(AnalyzeContext ctx) {
		ArrayList<Literal> result = new ArrayList<>();

		result.add(makeListAtom(ctx,
								new Variable(node.getParent().varName()),
								new Variable(node.indexVarName()),
								new Variable(node.varName())));

		result.add(new EQLiteral(new PredicateSymbol("EQ"),
								 new Variable(node.indexVarName()),
								 makeAdd(new Variable(predecessor.indexVarName()),
										 new IntConstant("1"))));

		return result;
	}
}
