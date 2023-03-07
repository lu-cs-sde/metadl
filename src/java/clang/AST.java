package clang;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AST {
	public static class TextualType {
		public String qualType = "";
	}

	public static class BareLoc {
		public int line;
		public int col;
		public String file;

		/**
		   Clang AST contains 'differential' source locations and ranges: it contains
		   only the changes relative to the previous node.
		   This propagates the location, so that each node has full location description.
		*/
		public BareLoc patch(BareLoc l) {
			if (l == null)
				return this;
			if (line == 0)
				line = l.line;
			if (col == 0)
				col = l.col;
			if (file == null)
				file = l.file;
			return this;
		}

		public String pretty() {
			return String.format("%s, %d:%d", file != null ? file : "UNKNOWN", line, col);
		}

		public String getFile() {
			if (file == null)
				return "UNKNOWN";
			return file;
		}
	}

	public static class Loc {
		public BareLoc spellingLoc;
		public BareLoc expansionLoc;
		public BareLoc loc;

		public boolean isMacroExpansion() {
			return spellingLoc != null || expansionLoc != null;
		}

		private BareLoc locOrder() {
			if (loc != null)
				return loc;
			if (spellingLoc != null)
				return spellingLoc;
			if (expansionLoc != null)
				return expansionLoc;
			return null;
		}

		public String pretty() {
			BareLoc loc = locOrder();
			if (loc != null)
				return loc.pretty();
			return "UNKNOWN";
		}

		public String getFile() {
			BareLoc loc = locOrder();
			if (loc != null)
				return loc.getFile();
			return "UNKNOWN";
		}

		public int getLine() {
			BareLoc loc = locOrder();
			if (loc != null)
				return loc.line;
			return 0;
		}

		public int getCol() {
			BareLoc loc = locOrder();
			if (loc != null)
				return loc.col;
			return 0;
		}
	}

	public static class Range {
		public Loc begin;
		public Loc end;

		public String pretty() {
			if (begin == null || end == null) {
				return "UNKNOWN";
			}
			return String.format("%s, %d:%d-%d:%d", begin.getFile(), begin.getLine(), begin.getCol(), end.getLine(), end.getCol());
		}
	}

	public static class Node {
		public String id;
		public String kind;
		public Loc loc;
		public Range range;
		public Node[] inner = {};

		public void prettyPrint(PrintStream ps) {
			prettyPrintInternal(0, ps);
		}

		public java.util.List<Node> children() {
			if (inner == null)
				return Collections.emptyList();
			return Arrays.asList(inner);
		}

		private void prettyPrintInternal(int offset, PrintStream ps) {
			for (int i = 0; i < offset; ++i) {
				ps.print("  ");
			}

			if (loc != null) {
				ps.printf("<%s> %s %s[%s]\n", id, kind, extraInfo(), loc.pretty());
			} else {
				ps.printf("<%s> %s %s[BUILTIN]\n", id, kind, extraInfo());
			}

			for (Node c : children()) {
				if (c != null)
					c.prettyPrintInternal(offset + 1, ps);
			}
		}

		protected String extraInfo() {
			return "";
		}

		public void accept(ASTVisitor visitor) { visitor.visit(this); }

		public void acceptPO(ASTVisitor visitor) {
			for (Node c : children()) {
				if (c != null)
					c.acceptPO(visitor);
			}
			accept(visitor);
		}

		protected <T extends Node> T setChildren(Node ... children) {
			this.inner = children;
			this.kind = this.getClass().getSimpleName();
			this.id = "0x" + Integer.toHexString(System.identityHashCode(this));
			return (T) this;
		}
	}


	public static Collection<Class> getASTNodeTypes() {
		// return all the (strict) subclasses of ClangAST.Node that are defined in this class
		return Arrays.stream(AST.class.getClasses())
				.filter(c -> AST.Node.class.isAssignableFrom(c) && c != AST.Node.class)
				.collect(Collectors.toUnmodifiableList());
	}

	// --------------------------------------------------------------------------------
	// Classes extending Node should mirror the clang node hierarchy
	// --------------------------------------------------------------------------------

	// --------------------------------------------------------------------------------
	// Expressions
	// --------------------------------------------------------------------------------
	public static class Expr extends Stmt {
		@Override protected String extraInfo() {
			return "EXPR";
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class CallExpr extends Expr {
		public Expr getCallee() {
			return (Expr) inner[0];
		}

		public int getNumArgs() {
			return inner.length - 1;
		}

		public Expr getArg(int i) {
			return (Expr) inner[i + 1];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class BinaryOperator extends Expr {
		public String opcode;

		public Expr getLHS() {
			return (Expr) inner[0];
		}

		public Expr getRHS() {
			return (Expr) inner[1];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class CompoundAssignOperator extends BinaryOperator {
		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class ConditionalOperator extends Expr {
		public Expr getCond() {
			return (Expr) inner[0];
		}

		public Expr getTrueExpr() {
			return (Expr) inner[1];
		}

		public Expr getFalseExpr() {
			return (Expr) inner[2];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class UnaryOperator extends Expr {
		public String opcode;
		public boolean isPostfix;

		public Expr getOperand() {
			return (Expr) inner[0];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class DeclRefExpr extends Expr {
		public Decl referencedDecl;

		public Decl getDecl() {
			return (Decl) referencedDecl;
		}

		@Override protected String extraInfo() {
			return "'" + referencedDecl.name + "' ";
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class ImplicitCastExpr extends Expr {
		public Expr getOperand() {
			return (Expr) inner[0];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class ExplicitCastExpr extends Expr {
		public Expr getOperand() {
			return (Expr) inner[0];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class IntegerLiteral extends Expr {
		public String value;

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class ArraySubscriptExpr extends Expr {
		public Expr getLHS() {
			return (Expr) inner[0];
		}

		public Expr getRHS() {
			return (Expr) inner[1];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class MetaExpr extends Expr {
		private String name;

		public MetaExpr(String name) {
			this.name = name;
		}

		public MetaExpr() {
		}

		public boolean isNamed() {
			return name != null;
		}

		public String getName() {
			assert isNamed();
			return name;
		}
	}

	// --------------------------------------------------------------------------------
	// Statements
	// --------------------------------------------------------------------------------
	public static class Stmt extends Node {
		@Override protected String extraInfo() {
			return "STMT";
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
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

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static ForStmt build(Stmt init, Expr cond, Expr incr, Stmt body) {
			return new ForStmt().setChildren(init, null, cond, incr, body);
		}
	}

	public static class CompoundStmt extends Stmt {
		public int getNumStmts() {
			return inner == null ? 0 : inner.length;
		}

		public Stmt getStmt(int i) {
			return (Stmt) inner[i];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static CompoundStmt build(Stmt ... stmts) {
			return new CompoundStmt().setChildren(stmts);
		}
	}

	public static class ReturnStmt extends Stmt {
		public Expr getRetValue() {
			return inner != null ? (Expr) inner[0] : null;
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static ReturnStmt build() {
			return new ReturnStmt().setChildren();
		}

		public static ReturnStmt build(Expr ret) {
			return new ReturnStmt().setChildren(ret);
		}
	}

	public static class DeclStmt extends Stmt {
		public Decl getDecl(int i) {
			return (Decl) inner[i];
		}

		public int getNumDecl() {
			return inner.length;
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class IfStmt extends Stmt {
		public boolean hasInit;
		public boolean hasVar;
		public boolean hasElse;

		public Stmt getInit() {
			if (hasInit)
				return (Stmt) inner[0];
			return null;
		}

		public Stmt getVar() {
			if (hasVar)
				return (Stmt) inner[hasInit ? 1 : 0];
			return null;
		}

		public Expr getCond() {
			return (Expr) inner[(hasInit ? 1 : 0) + (hasVar ? 1 : 0)];
		}

		public Stmt getThen() {
			return (Stmt) inner[1 + (hasInit ? 1 : 0) + (hasVar ? 1 : 0)];
		}

		public Stmt getElse() {
			if (hasElse)
				return (Stmt) inner[2 + (hasInit ? 1 : 0) + (hasVar ? 1 : 0)];
			return null;
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class WhileStmt extends Stmt {
		public Stmt getBody() {
			return (Stmt) inner[inner.length - 1];
		}

		public Stmt getCond() {
			return (Stmt) inner[inner.length - 2];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static WhileStmt build(Stmt cond, Stmt body) {
			return new WhileStmt().setChildren(cond, body);
		}
	}

	public static class DoStmt extends Stmt {
		public Stmt getBody() {
			return (Stmt) inner[0];
		}

		public Expr getCond() {
			return (Expr) inner[1];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static DoStmt build(Stmt cond, Stmt body) {
			return new DoStmt().setChildren(cond, body);
		}
	}

	public static class CXXForRangeStmt extends Stmt {
		public Stmt getBody() {
			return (Stmt) inner[7];
		}

		public VarDecl getLoopVariable() {
			return (VarDecl) inner[6];
		}

		public DeclStmt getRangeStmt() {
			return (DeclStmt) inner[1];
		}

		public Stmt getInit() {
			return (Stmt) inner[0];
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class MetaStmt extends Stmt {
		private String name;

		public MetaStmt(String name) {
			this.name = name;
		}

		public MetaStmt() {
		}

		public boolean isNamed() {
			return name != null;
		}

		public String getName() {
			assert isNamed();
			return name;
		}
	}

	//--------------------------------------------------------------------------------
	// Declarations
	//--------------------------------------------------------------------------------
	public static class Decl extends Node {
		public String name = "";
		public TextualType type;
		public Type explicitType;

		public boolean isNamed() {
			return !name.isEmpty();
		}

		public String getName() {
			return name;
		}

		@Override protected String extraInfo() {
			String ret = "'" + name + "' : ";
			if (explicitType != null) {
				ByteArrayOutputStream os = new ByteArrayOutputStream();
				explicitType.prettyPrint(new PrintStream(os));
				ret += "{" + os.toString() + "}";
			} else if (type != null) {
				ret += type.qualType;
			} else {
				ret += "?";
			}
			return ret;
		}

		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class ParmVarDecl extends VarDecl {
		@Override public void accept(ASTVisitor v) {
			v.visit(this);
		}


		public static ParmVarDecl build(String name, Type t) {
			ParmVarDecl ret = new ParmVarDecl();
			ret.name = name;
			ret.explicitType = t;
			return ret.setChildren();
		}

		public static ParmVarDecl build(Type t) {
			ParmVarDecl ret = new ParmVarDecl();
			ret.explicitType = t;
			return ret.setChildren();
		}
	}

	public static class FunctionDecl extends Decl {
		public String mangledName = "";
		public boolean variadic;

		@Override protected String extraInfo() {
			return String.format(" mangledName:'%s' ", mangledName) + super.extraInfo();
		}

		private List<ParmVarDecl> params;
		public List<ParmVarDecl> getParams() {
			if (params == null) {
				// compute the indices of the first and last ParmVarDecl:s in the list
				int firstParamIdx;
				for (firstParamIdx = 0; firstParamIdx < inner.length; firstParamIdx++) {
					if (inner[firstParamIdx] instanceof ParmVarDecl)
						break;
				}

				int lastParamIdx;
				for (lastParamIdx = inner.length - 1; lastParamIdx >= 0; lastParamIdx--) {
					if (inner[lastParamIdx] instanceof ParmVarDecl) {
						break;
					}
				}


				if (firstParamIdx <= lastParamIdx) {
					params = Arrays.asList(inner).subList(firstParamIdx, lastParamIdx + 1).stream()
						.map((AST.Node n) -> (AST.ParmVarDecl) n)
						.collect(Collectors.toUnmodifiableList());
				} else {
					params = Collections.emptyList();
				}
			}

			return params;
		}

		public Stmt getBody() {
			if (inner != null && inner.length > 0 && inner[inner.length - 1] instanceof Stmt) {
				return (Stmt) inner[inner.length - 1];
			}
			return null;
		}

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static FunctionDecl build(String name, Type t, List<ParmVarDecl> params) {
			FunctionDecl ret = new FunctionDecl();
			ret.mangledName = name;
			ret.explicitType = t;
			return ret.setChildren(params.toArray(new Node[0]));
		}

		public static FunctionDecl build(String name, Type t, List<ParmVarDecl> params, Stmt body) {
			FunctionDecl ret = new FunctionDecl();
			ret.mangledName = name;
			ret.explicitType = t;
			return ret.setChildren(Stream.concat(params.stream(), List.of(body).stream()).toArray(Node[]::new));
		}
	}

	public static class VarDecl extends Decl {
		public String init; // c, call or list
		public String storageClass;

		public Expr getInit() {
			if (init != null)
				return (Expr) inner[0];
			return null;
		}

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static VarDecl build(String name, Type t) {
			VarDecl ret = new VarDecl();
			ret.name = name;
			ret.explicitType = t;
			return ret.setChildren();
		}

		public static VarDecl build(String name, Type t, Expr init) {
			return build(name, t).setChildren(init);
		}
	}

	public static class FieldDecl extends Decl {
		boolean isBitfield;

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public Expr getBitFieldSize() {
			if (isBitfield)
				return (Expr) inner[0];
			return null;
		}
	}

	public static class TranslationUnitDecl extends Decl {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public int getNumDecl() {
			return inner.length;
		}

		public Decl getDecl(int i) {
			return (Decl) inner[i];
		}
	}

	public static Decl toDecl(Node n) {
		if (n instanceof Decl) {
			return (Decl) n;
		}
		return null;
	}


	public static class RecordDecl extends Decl {
		public String tagUsed;
		public boolean completeDefinition;

		public int getNumDecl() {
			return inner.length;
		}

		public Decl getDecl(int i) {
			return toDecl(inner[i]);
		}

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

	}

	public static class TypedefDecl extends Decl {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class CXXMethodDecl extends FunctionDecl {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	public static class MetaDecl extends Decl {
		private String name;

		public MetaDecl(String name) {
			this.name = name;
		}

		public MetaDecl() {
		}

		public boolean isNamed() {
			return name != null;
		}

		public String getName() {
			assert isNamed();
			return name;
		}
	}


	//--------------------------------------------------------------------------------
	// Types
	//--------------------------------------------------------------------------------
	public static class Type extends Node {
		TextualType type;
		StorageClass storage = StorageClass.AUTO;
		List<TypeQualifier> typeQuals = Collections.emptyList();

		@Override public String extraInfo() {
			String ret = "";
			if (storage != StorageClass.AUTO) {
				ret += storage;
			}

			for (TypeQualifier tq : typeQuals) {
				ret += " " + tq;
			}

			return ret;
		}

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public void setTypeQuals(List<TypeQualifier> quals) {
			for (TypeQualifier q : quals) {
				addQual(q);
			}
		}

		public void setStorage(StorageClass sc) {
			storage = sc;
		}

		public void addQual(TypeQualifier q) {
			if (typeQuals.isEmpty()) {
				typeQuals = new ArrayList<>();
			}
			typeQuals.add(q);
		}
	}

	public static class PointerType extends Type {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static PointerType build(Type pointeeType) {
			return new PointerType().setChildren(pointeeType);
		}

		public Type getPointeeType() {
			return (Type) inner[0];
		}
	}

	public static class ArrayType extends Type {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public static ArrayType build(Type elementType) {
			return new ArrayType().setChildren(elementType);
		}

		public Type getElementType() {
			return (Type) inner[0];
		}
	}

	public static class ParenType extends Type {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public Type getInnerType() {
			if (inner != null && inner.length > 0) {
				return (Type) inner[0];
			}
			return null;
		}

		public static ParenType build(Type innerType) {
			return new ParenType().setChildren(innerType);
		}
	}

	public static class FunctionProtoType extends Type {
		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public Type getReturnType(ASTVisitor v) {
			if (inner != null && inner.length > 0) {
				return (Type) inner[0];
			}
			return null;
		}

		public int getNumParamType() {
			return Integer.max(inner.length - 1, 0);
		}

		public Type getParamType(int i) {
			return (Type) inner[i - 1];
		}

		public static FunctionProtoType build(Type retType, List<Type> args) {
			FunctionProtoType ret = new FunctionProtoType().setChildren();

			ret.inner = new Node[args.size() + 1];
			ret.inner[0] = retType;
			for (int i = 0; i < args.size(); ++i) {
				ret.inner[i + 1] = args.get(i);
			}
			return ret;
		}
	}

	public enum BuiltinTypeKind {
		VOID,
		CHAR,
		SHORT,
		INT,
		LONG,
		FLOAT,
		DOUBLE,
		SIGNED,
		UNSIGNED,
		BOOL,
		COMPLEX
	}

	public enum StorageClass {
		EXTERN,
		STATIC,
		THREAD_LOCAL,
		AUTO,
		REGISTER,
		NONE
	}

	public enum TypeQualifier {
		CONST,
		RESTRICT,
		VOLATILE,
		ATOMIC,
		NONE
	}

	public static class BuiltinType extends Type {
		public List<BuiltinTypeKind> kinds = Collections.emptyList();

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		@Override public String extraInfo() {
			String ret = super.extraInfo();
			for (BuiltinTypeKind kind : kinds) {
				ret += " " + kind ;
			}
			return ret;
		}

		public static BuiltinType build(List<BuiltinTypeKind> kinds) {
			BuiltinType ret = new BuiltinType();
			ret.kinds = kinds;
			return ret.setChildren();
		}
	}

	//--------------------------------------------------------------------------------
	// Comments (Documentation comments)
	//--------------------------------------------------------------------------------
	public static class Comment extends Decl {
		public String text = "";
		public void accept(ASTVisitor v) {
			v.visit(this);
		}
	}

	//--------------------------------------------------------------------------------
	// Placeholder nodes / Metavariables
	//--------------------------------------------------------------------------------
	interface MetaVariable {
		String getName();
		boolean isWildcard();
	}

	public static class TypeMetaVariable extends Type implements MetaVariable {
		String name;

		public static TypeMetaVariable build() {
			return new TypeMetaVariable();
		}

		public static TypeMetaVariable build(String name) {
			TypeMetaVariable ret = new TypeMetaVariable();
			ret.name = name;
			return ret;
		}

		public void accept(ASTVisitor v) {
			v.visit(this);
		}

		public String getName() {
			if (isWildcard()) {
				throw new RuntimeException("Wildcards do not have names.");
			}
			return name;
		}

		public boolean isWildcard() {
			return name == null;
		}
	}
}
