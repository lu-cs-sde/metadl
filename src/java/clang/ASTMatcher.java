package clang;

import java.util.Collections;
import java.util.List;

public class ASTMatcher {
	protected List<ASTMatcher> children;
	private String name;


	public String genMatcher() {
		String ret = name;
		ret += "(";


		boolean isFirst = true;
		for (ASTMatcher m : children) {
			if (!isFirst) {
				ret += ", ";
			} else {
				isFirst = false;
			}
			ret += m.genMatcher();
		}

		ret += ")";
		return ret;
	}

	public ASTMatcher(String name, List<ASTMatcher> children) {
		this.name = name;
		this.children = children;
	}

	public ASTMatcher(String name) {
		this(name, Collections.emptyList());
	}

	public ASTMatcher(String name, ASTMatcher m) {
		this(name, Collections.singletonList(m));
	}

	public ASTMatcher bind(String var) {
		return new ASTMatcher("bind", this) {
			@Override public String genMatcher() {
				return this.children.get(0).genMatcher() + ".bind(\"" + var + "\")";
			}
		};
	}

	// Exprs

	public static ASTMatcher expr(List<ASTMatcher> m) {
		return new ASTMatcher("expr", m);
	}

	public static ASTMatcher callExpr(List<ASTMatcher> m) {
		return new ASTMatcher("callExpr", m);
	}



	public static ASTMatcher functionDecl(List<ASTMatcher> m) {
		return new ASTMatcher("functionDecl", m);
	}

	public static ASTMatcher recordDecl(List<ASTMatcher>  m) {
		return new ASTMatcher("recordDecl", m);
	}

	// Stmts

	public static ASTMatcher forStmt(List<ASTMatcher> m) {
		return new ASTMatcher("forStmt", m);
	}

	public static ASTMatcher hasLoopInit(ASTMatcher m) {
		return new ASTMatcher("hasLoopInit", m);
	}

	public static ASTMatcher hasCondition(ASTMatcher m) {
		return new ASTMatcher("hasCondition", m);
	}

	public static ASTMatcher hasIncrement(ASTMatcher m) {
		return new ASTMatcher("hasIncrement", m);
	}

	public static ASTMatcher hasBody(ASTMatcher m) {
		return new ASTMatcher("hasBody", m);
	}

	public static ASTMatcher whileStmt(List<ASTMatcher> m) {
		return new ASTMatcher("whileStmt", m);
	}

	public static ASTMatcher doStmt(List<ASTMatcher> m) {
		return new ASTMatcher("doStmt", m);
	}

	public static ASTMatcher nullStmt() {
		return new ASTMatcher("nullStmt");
	}

	public static ASTMatcher stmt(List<ASTMatcher> m) {
		return new ASTMatcher("stmt", m);
	}

	// Decls
	public static ASTMatcher isDefinition() {
		return new ASTMatcher("isDefinition");
	}




}
