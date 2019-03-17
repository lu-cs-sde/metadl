package lang.cons;

import lang.ast.*;
import java.util.ArrayList;

public abstract class Constraint {
	public Clause generateClause(String scopePrefix) {return null;};
	public abstract ArrayList<Literal> generateLiterals(String scopePrefix);
	protected ASTNode node;
	protected Constraint(ASTNode node) {
		this.node = node;
	}

	protected static Atom makeAtom(String pred, Term... ts) {
		PredicateSymbol ps = new PredicateSymbol(pred);
		List<Term> terms = new List<>(ts);
		return new Atom(ps, terms);
	}


	public static Constraint member(ASTNode n) {
		return new MemberConstraint(n);
	}

	public static Constraint eq(ASTNode n1, int pos) {
		return new EqualityConstraint(n1, pos);
	}

	public static Constraint last(ASTNode n) {
		return new LastConstraint(n);
	}

	public static Constraint after(ASTNode n, ASTNode after) {
		return new AfterConstraint(after, n);
	}

	public static Constraint next(ASTNode n, ASTNode next) {
		return new NextConstraint(next, n);
	}
}

class EqualityConstraint extends Constraint {
	private int pos;
	public EqualityConstraint(ASTNode node, int pos) {
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
	public MemberConstraint(ASTNode node) {
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
	public LastConstraint(ASTNode node) {
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
								   new AddExpr(new Variable(node.indexVarName()),
											   new IntConstant("1"))));
		result.add(new NEGLiteral(new PredicateSymbol("NEG"),
								  makeAtom(scopePrefix + newRuleName,
										   new Variable(node.getParent().varName()),
										   new Variable(boundVarName))));
		return result;
	}
}

class AfterConstraint extends Constraint {
	private ASTNode predecessor;

	public AfterConstraint(ASTNode node, ASTNode predecessor) {
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
	private ASTNode predecessor;
	public NextConstraint(ASTNode node, ASTNode predecessor) {
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
								 new AddExpr(new Variable(predecessor.indexVarName()),
											 new IntConstant("1"))));

		return result;
	}
}
