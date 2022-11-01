package clang;

import static clang.AST.*;

public interface ASTVisitor {
	void visit(Node n);
	void visit(CallExpr e);
	void visit(BinaryOperator b);
	void visit(CompoundAssignOperator c);
	void visit(ConditionalOperator c);
	void visit(UnaryOperator u);
	void visit(DeclRefExpr e);
	void visit(ExplicitCastExpr e);
	void visit(ImplicitCastExpr e);
	void visit(IntegerLiteral n);

	void visit(Stmt s);
	void visit(ForStmt f);
	void visit(CompoundStmt c);
	void visit(ReturnStmt r);
	void visit(DeclStmt d);

	void visit(Decl d);
	void visit(ParmVarDecl p);
	void visit(FunctionDecl f);
	void visit(VarDecl d);
	void visit(TranslationUnitDecl tu);
}
