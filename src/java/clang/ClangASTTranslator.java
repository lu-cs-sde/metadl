package clang;

import java.util.Map;
import java.util.HashMap;
import java.util.function.BiFunction;

import clang.ClangAST.BinaryOperator;
import clang.ClangAST.CallExpr;
import clang.ClangAST.CompoundAssignOperator;
import clang.ClangAST.CompoundStmt;
import clang.ClangAST.ConditionalOperator;
import clang.ClangAST.DeclRefExpr;
import clang.ClangAST.ForStmt;
import clang.ClangAST.FunctionDecl;
import clang.ClangAST.Node;
import clang.ClangAST.ParmVarDecl;
import clang.ClangAST.ReturnStmt;
import clang.ClangAST.UnaryOperator;
import lang.c.obj.ast.*;

public abstract class ClangASTTranslator implements ASTVisitor {
	private Map<ClangAST.Node, ASTNode> nodeMap = new HashMap<>();

	private void t(ClangAST.Node node, ASTNode internalNode) {
		nodeMap.put(node, internalNode);
	}

	private <T> T t(ClangAST.Node node) {
		return (T) nodeMap.get(node);
	}

	@Override public void visit(Node n) { }
	@Override public void visit(CallExpr e) {
		Expression callee = t(e.getCallee());
		List<Expression> args = new List<>();
		for (int i = 0; i < e.getNumArgs(); ++i)
			args.add(t(e.getArg(i)));
		t(e, new CallExpression(callee, args));
	}

	@Override public void visit(BinaryOperator b) {
		Expression lhs = t(b.getLHS());
		Expression rhs = t(b.getRHS());


		BiFunction<Expression, Expression, Expression> c = null;

		switch (b.opcode) {
		case "+": c = AddExpression::new; break;
		case "-": c = SubExpression::new; break;
		case "*": c = MulExpression::new; break;
		case "/": c = DivExpression::new; break;
		case "%": c = ModExpression::new; break;
		case ">>": c = RShiftExpression::new; break;
		case "<<": c = LShiftExpression::new; break;
		case "<": c = LTExpression::new; break;
		case ">": c= GTExpression::new; break;
		case "<=": c = LEQExpression::new; break;
		case ">=": c = GEQExpression::new; break;
		case "==": c = EQExpression::new; break;
		case "!=": c = NEQExpression::new; break;
		case "&" : c = BitwiseAndExpression::new; break;
		case "|" : c = BitwiseOrExpression::new; break;
		case "&&" : c = AndExpression::new; break;
		case "||" : c = OrExpression::new; break;
		default: throw new RuntimeException("Opcode not yet supported: " + b.opcode);
		}
		t(b, c.apply(lhs, rhs));
	}

	@Override public void visit(CompoundAssignOperator c) { }
	@Override public void visit(ConditionalOperator c) { }
	@Override public void visit(UnaryOperator u) {
	}
	@Override public void visit(DeclRefExpr e) {
		t(e, new IdentifierExpression(new Identifier(e.getDecl().name)));
	}
	@Override public void visit(ForStmt f) {

	}
	@Override public void visit(CompoundStmt c) {

	}
	@Override public void visit(ReturnStmt r) {

	}
	@Override public void visit(ParmVarDecl p) {

	}
	@Override public void visit(FunctionDecl f) {
	}
}
