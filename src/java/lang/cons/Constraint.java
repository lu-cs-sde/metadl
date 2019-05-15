package lang.cons;

import lang.ast.*;
import java.util.ArrayList;

public abstract class Constraint {
	public Clause generateClause(String scopePrefix) {return null;};
	public abstract ArrayList<Literal> generateLiterals(String scopePrefix);
	protected ObjLangASTNode node;
	protected Constraint(ObjLangASTNode node) {
		this.node = node;
	}

	protected static Atom makeAtom(String pred, Term... ts) {
		PredicateSymbol ps = new PredicateSymbol(pred);
		List<Term> terms = new List<>(ts);
		return new Atom(ps, terms);
	}

	protected Expr makeAdd(Expr left, Expr right) {
		List<Expr> args = new List<>();
		args.add(left);
		args.add(right);
		return new Functor(new FunctorSymbol("add"), args);
	}

	public static Constraint member(ObjLangASTNode n) {
		return new MemberConstraint(n);
	}

	public static Constraint eq(ObjLangASTNode n1, int pos) {
		return new EqualityConstraint(n1, pos);
	}

	public static Constraint last(ObjLangASTNode n) {
		return new LastConstraint(n);
	}

	public static Constraint after(ObjLangASTNode n, ObjLangASTNode after) {
		return new AfterConstraint(after, n);
	}

	public static Constraint next(ObjLangASTNode n, ObjLangASTNode next) {
		return new NextConstraint(next, n);
	}
}

class EqualityConstraint extends Constraint {
	private int pos;
	public EqualityConstraint(ObjLangASTNode node, int pos) {
		super(node);
		this.pos = pos;
	}
	public ArrayList<Literal> generateLiterals(String scopePrefix) {
		ArrayList<Literal> result = new ArrayList<>();
		result.add(makeAtom(scopePrefix + super.node.getParent().getRelation(),
							new Variable(node.getParent().varName()),
							new Variable(node.indexVarName()),
							new Variable(node.varName())));

		result.add(new EQLiteral(new PredicateSymbol("EQ"),
								 new Variable(node.indexVarName()),
								 new IntConstant("" + pos)));
		return result;
	}
}

class MemberConstraint extends Constraint {
	ArrayList<Literal> result = new ArrayList<>();
	final String boundVarName = "b_" + node.getNodeId();
	public MemberConstraint(ObjLangASTNode node) {
		super(node);
	}
	public ArrayList<Literal> generateLiterals(String scopePrefix) {
		ArrayList<Literal> result = new ArrayList<>();
		result.add(makeAtom(scopePrefix + node.getParent().getRelation(),
							new Variable(node.getParent().varName()),
							new Variable(node.indexVarName()),
							new Variable(node.varName())));
		return result;
	}
}

class LastConstraint extends Constraint {
	final String newRuleName = node.getParent().getRelation() + "_proj";
	// This element must be the last one in the list.
	// Check that the relation List(l, _, i+1) is empty.
	// List1(l, i) :- List(l, i, c).
	// List(l, i, child), BIND(j, i+1), NEG(List1(l, j)).
	public LastConstraint(ObjLangASTNode node) {
		super(node);
	}

	@Override
	public Clause generateClause(String scopePrefix) {
		Atom list1 = makeAtom(scopePrefix + newRuleName,
							  new Variable("l"),
							  new Variable("i"));
		Atom list = makeAtom(scopePrefix + node.getParent().getRelation(),
							 new Variable("l"),
							 new Variable("i"),
							 new Variable("x"));
		Rule r = new Rule(new List<>(list1), new List<>(list));
		return r;
	}

	public ArrayList<Literal> generateLiterals(String scopePrefix) {
		ArrayList<Literal> result = new ArrayList<>();
		String boundVarName = "b_" + node.getNodeId();
		result.add(makeAtom(scopePrefix + node.getParent().getRelation(),
							new Variable(node.getParent().varName()),
							new Variable(node.indexVarName()),
							new Variable(node.varName())));
		result.add(new BINDLiteral(new PredicateSymbol("BIND"),
								   new Variable(boundVarName),
								   makeAdd(new Variable(node.indexVarName()),
										   new IntConstant("1"))));
		result.add(new NEGLiteral(new PredicateSymbol("NEG"),
								  makeAtom(scopePrefix + newRuleName,
										   new Variable(node.getParent().varName()),
										   new Variable(boundVarName))));
		return result;
	}
}

class AfterConstraint extends Constraint {
	private ObjLangASTNode predecessor;

	public AfterConstraint(ObjLangASTNode node, ObjLangASTNode predecessor) {
		super(node);
		this.predecessor = predecessor;
	}

	public ArrayList<Literal> generateLiterals(String scopePrefix) {
		ArrayList<Literal> result = new ArrayList<>();

		result.add(makeAtom(scopePrefix + super.node.getParent().getRelation(),
							new Variable(node.getParent().varName()),
							new Variable(node.indexVarName()),
							new Variable(node.varName())));

		result.add(new GTLiteral(new PredicateSymbol("GT"),
								 new Variable(node.indexVarName()),
								 new Variable(predecessor.indexVarName())));

		return result;
	}
}

class NextConstraint extends Constraint {
	private ObjLangASTNode predecessor;
	public NextConstraint(ObjLangASTNode node, ObjLangASTNode predecessor) {
		super(node);
		this.predecessor = predecessor;
	}

	public ArrayList<Literal> generateLiterals(String scopePrefix) {
		ArrayList<Literal> result = new ArrayList<>();

		result.add(makeAtom(scopePrefix + super.node.getParent().getRelation(),
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
