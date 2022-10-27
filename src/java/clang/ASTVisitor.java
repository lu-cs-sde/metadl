package clang;

import static clang.ClangAST.*;

public interface ASTVisitor {
	void visit(Node n);
	void visit(CallExpr e);
	void visit(BinaryOperator b);
	void visit(CompoundAssignOperator c);
	void visit(ConditionalOperator c);
	void visit(UnaryOperator u);
	void visit(DeclRefExpr e);

	void visit(ForStmt f);
	void visit(CompoundStmt c);
	void visit(ReturnStmt r);

	void visit(ParmVarDecl p);
	void visit(FunctionDecl f);
}
