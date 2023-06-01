package clang;

import static clang.AST.*;

public interface ASTVisitor {
	void visit(Node n);

	default void visit(Expr e) { visit((Node) e); }
	default void visit(CallExpr e){ visit((Node) e); }
	default void visit(BinaryOperator b){ visit((Node) b); }
	default void visit(CompoundAssignOperator c){ visit((Node) c); }
	default void visit(ConditionalOperator c){ visit((Node) c); }
	default void visit(UnaryOperator u){ visit((Node) u); }
	default void visit(DeclRefExpr e){ visit((Node) e); }
	default void visit(ExplicitCastExpr e){ visit((Node) e); }
	default void visit(ImplicitCastExpr e){ visit((Node) e); }
	default void visit(IntegerLiteral n){ visit((Node) n); }
	default void visit(ArraySubscriptExpr a){ visit((Node) a); }

	default void visit(Stmt s){ visit((Node) s); }
	default void visit(ForStmt f){ visit((Node) f); }
	default void visit(WhileStmt w){ visit((Node) w); }
	default void visit(DoStmt d){ visit((Node) d); }
	default void visit(CompoundStmt c){ visit((Node) c); }
	default void visit(ReturnStmt r){ visit((Node) r); }
	default void visit(DeclStmt d){ visit((Node) d); }
	default void visit(IfStmt f){ visit((Node) f); }
	default void visit(CXXForRangeStmt f){ visit((Node) f); }
  default void visit(SwitchStmt s) { visit((Node) s); }
  default void visit(CaseStmt s) { visit((Node) s); }
  default void visit(DefaultStmt s) { visit((Node) s); }
  default void visit(BreakStmt s) { visit((Node) s); }

	default void visit(Decl d){ visit((Node) d); }
	default void visit(ParmVarDecl p){ visit((Node) p); }
	default void visit(FunctionDecl f){ visit((Node) f); }
	default void visit(VarDecl d){ visit((Node) d); }
	default void visit(RecordDecl d){ visit((Node) d); }
	default void visit(FieldDecl f){ visit((Node) f); }
	default void visit(TranslationUnitDecl u){ visit((Node) u); }

	default void visit(Type t){ visit((Node) t); }
	default void visit(PointerType t){ visit((Node) t); }
	default void visit(ParenType t){ visit((Node) t); }
	default void visit(FunctionProtoType t){ visit((Node) t); }
  default void visit(FunctionNoProtoType t){ visit((Node) t); }
	default void visit(BuiltinType t){ visit((Node) t); }
	default void visit(ArrayType t){ visit((Node) t); }
  default void visit(RecordRefType t){ visit((Node) t); }
  default void visit(TypedefType t){ visit((Node) t); }
	default void visit(Comment c){ visit((Node) c); }
}
