package clang;

import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.Collection;
import java.util.Iterator;
import java.util.stream.Collectors;

import org.apache.commons.collections4.iterators.ArrayIterator;
import org.apache.commons.lang3.mutable.MutableObject;

public class ClangAST {
	public static class Type {
		public String qualType = "";
	}


	public static class Loc {
		public int line;
		public int col;
		public String file = "<UNKNOWN>";

		public Loc patch(Loc l) {
			if (l == null)
				return this;
			if (line == 0)
				line = l.line;
			if (col == 0)
				col = l.col;
			if (file == null || file.equals("<UNKNOWN>"))
				file = l.file;
			return this;
		}

		public Loc(Loc l) {
			this.line = l.line;
			this.col = l.col;
			this.file = l.file;
		}
	}

	public static class Range {
		public Loc begin;
		public Loc end;
	}

	public static class Node {
		public String id;
		public String kind;
		public Loc loc;
		public Range range;
		public Node[] inner;

		public void prettyPrint(PrintStream ps) {
			prettyPrintInternal(0, ps);
		}

		public Iterator<Node> children() {
			if (inner == null)
				return Collections.emptyIterator();
			return new ArrayIterator<Node>(inner);
		}

		private void prettyPrintInternal(int offset, PrintStream ps) {
			for (int i = 0; i < offset; ++i) {
				ps.print("  ");
			}

			if (loc != null) {
				ps.printf("<%s> %s %s[%s, %d:%d-%d:%d]\n", id, kind, extraInfo(), loc.file,
						  range.begin.line, range.begin.col, range.end.line, range.end.col);
			} else {
				ps.printf("<%s> %s %s[BUILTIN]\n", id, kind, extraInfo());
			}

			children().forEachRemaining(c -> c.prettyPrintInternal(offset + 1, ps));
		}

		protected String extraInfo() {
			return "";
		}

		private void patchLocationsInternal(MutableObject<Loc> prev) {
			if (loc != null)
				prev.setValue(loc.patch(prev.getValue()));
			else
				loc = new Loc(prev.getValue());

			if (range != null) {
				if (range.begin != null)
					range.begin.patch(prev.getValue());
				else
					range.begin = new Loc(prev.getValue());

				prev.setValue(range.begin);

				if (range.end != null)
					range.end.patch(prev.getValue());
				else
					range.end = new Loc(range.begin);

				prev.setValue(range.end);
			} else {
				range = new Range();
				range.begin = new Loc(prev.getValue());
				range.end = new Loc(prev.getValue());
			}

			Iterator<Node> c = children();
			while (c.hasNext()) {
				c.next().patchLocationsInternal(prev);
			}
		}

		public void patchLocations() {
			patchLocationsInternal(new MutableObject<>());
		}
	}


	public static Collection<Class> getASTNodeTypes() {
		// return all the (strict) subclasses of ClangAST.Node that are defined in this class
		return Arrays.stream(ClangAST.class.getClasses())
				.filter(c -> ClangAST.Node.class.isAssignableFrom(c) && c != ClangAST.Node.class)
				.collect(Collectors.toUnmodifiableList());
	}

	// --------------------------------------------------------------------------------
	// Classes extending Node should mirror the clang node hierarchy
	// --------------------------------------------------------------------------------

	// --------------------------------------------------------------------------------
	// Expressions
	// --------------------------------------------------------------------------------
	public static class Expr extends Node{
		@Override protected String extraInfo() {
			return "EXPR";
		}
	}

	public static class CallExpr extends Node {
		public Expr getCallee() {
			return (Expr) inner[0];
		}

		public int getNumArgs() {
			return inner.length - 1;
		}

		public Expr getArg(int i) {
			return (Expr) inner[i + 1];
		}
	}

	public static class BinaryOperator extends Expr {
		public String opcode;

		public Expr getLHS() {
			return (Expr) inner[0];
		}

		public Expr gerRHS() {
			return (Expr) inner[1];
		}
	}

	public static class CompoundAssignOperator extends BinaryOperator {

	}

	public class ConditionalOperator extends Expr {
		public Expr getCond() {
			return (Expr) inner[0];
		}

		public Expr getTrueExpr() {
			return (Expr) inner[0];
		}

		public Expr getFalseExpr() {
			return (Expr) inner[1];
		}
	}

	public static class UnaryOperator extends Expr {
		public String opcode;

		public Expr getOperand() {
			return (Expr) inner[0];
		}
	}

	public static class DeclRefExpr extends Expr {
		Decl referencedDecl;

		Decl getDecl() {
			return (Decl) referencedDecl;
		}

		@Override protected String extraInfo() {
			return "'" + referencedDecl.name + "' ";
		}
	}


	// --------------------------------------------------------------------------------
	// Statements
	// --------------------------------------------------------------------------------
	public static class Stmt extends Node {
		@Override protected String extraInfo() {
			return "STMT";
		}
	}

	public static class ForStmt extends Stmt {
		public Stmt getInit() {
			return (Stmt) inner[0];
		}

		public Expr getCond() {
			return (Expr) inner[2];
		}

		public Expr getIncr() {
			return (Expr) inner[3];
		}

		public Stmt getBody() {
			return (Stmt) inner[4];
		}
	}

	public static class CompoundStmt extends Stmt {
		public int getNumStmts() {
			return inner.length;
		}

		public Stmt getStmt(int i) {
			return (Stmt) inner[i];
		}
	}

	public static class ReturnStmt extends Stmt {
		public Expr getRetValue() {
			return inner != null ? (Expr) inner[0] : null;
		}
	}

	//--------------------------------------------------------------------------------
	// Declarations
	//--------------------------------------------------------------------------------
	public static class Decl extends Node {
		String name = "";

		@Override protected String extraInfo() {
			return "'" + name + "'";
		}
	}

	public static class ParmVarDecl extends Decl {
	}

	public static class FunctionDecl extends Node {
		public String mangledName = "";
		public Type type = new Type();

		@Override protected String extraInfo() {
			return String.format(" mangledName:'%s' ", mangledName);
		}

		public int getNumParams() {
			return 0;
		}

		public ParmVarDecl getParam() {
			return null;
		}

		public int getBody() {
			return 0;
		}
	}
}
