package clang;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

import clang.ClangAST.BinaryOperator;
import clang.ClangAST.CallExpr;
import clang.ClangAST.CompoundAssignOperator;
import clang.ClangAST.CompoundStmt;
import clang.ClangAST.ConditionalOperator;
import clang.ClangAST.Decl;
import clang.ClangAST.DeclRefExpr;
import clang.ClangAST.DeclStmt;
import clang.ClangAST.ExplicitCastExpr;
import clang.ClangAST.Expr;
import clang.ClangAST.ForStmt;
import clang.ClangAST.FunctionDecl;
import clang.ClangAST.ImplicitCastExpr;
import clang.ClangAST.IntegerLiteral;
import clang.ClangAST.Node;
import clang.ClangAST.ParmVarDecl;
import clang.ClangAST.ReturnStmt;
import clang.ClangAST.Stmt;
import clang.ClangAST.TranslationUnitDecl;
import clang.ClangAST.UnaryOperator;
import clang.ClangAST.VarDecl;
import lang.c.obj.ast.ASTNode;
import lang.c.obj.ast.AddExpression;
import lang.c.obj.ast.AddressOfExpression;
import lang.c.obj.ast.AndExpression;
import lang.c.obj.ast.AssignmentAddExpression;
import lang.c.obj.ast.AssignmentAndExpression;
import lang.c.obj.ast.AssignmentDivExpression;
import lang.c.obj.ast.AssignmentExpression;
import lang.c.obj.ast.AssignmentLShiftExpression;
import lang.c.obj.ast.AssignmentModExpression;
import lang.c.obj.ast.AssignmentMulExpression;
import lang.c.obj.ast.AssignmentOrExpression;
import lang.c.obj.ast.AssignmentRShiftExpression;
import lang.c.obj.ast.AssignmentSubExpression;
import lang.c.obj.ast.AssignmentXorExpression;
import lang.c.obj.ast.BitwiseAndExpression;
import lang.c.obj.ast.BitwiseNotExpression;
import lang.c.obj.ast.BitwiseOrExpression;
import lang.c.obj.ast.BitwiseXorExpression;
import lang.c.obj.ast.CallExpression;
import lang.c.obj.ast.CompoundStatement;
import lang.c.obj.ast.Constant;
import lang.c.obj.ast.ConstantExpression;
import lang.c.obj.ast.Declaration;
import lang.c.obj.ast.DivExpression;
import lang.c.obj.ast.EQExpression;
import lang.c.obj.ast.Expression;
import lang.c.obj.ast.ExpressionStatement;
import lang.c.obj.ast.ExternalDeclaration;
import lang.c.obj.ast.ExternalDeclarationOrDefinition;
import lang.c.obj.ast.ForStatement;
import lang.c.obj.ast.FunctionDefinition;
import lang.c.obj.ast.GEQExpression;
import lang.c.obj.ast.GTExpression;
import lang.c.obj.ast.Identifier;
import lang.c.obj.ast.IdentifierExpression;
import lang.c.obj.ast.LEQExpression;
import lang.c.obj.ast.LShiftExpression;
import lang.c.obj.ast.LTExpression;
import lang.c.obj.ast.List;
import lang.c.obj.ast.ModExpression;
import lang.c.obj.ast.MulExpression;
import lang.c.obj.ast.NEQExpression;
import lang.c.obj.ast.NotExpression;
import lang.c.obj.ast.Opt;
import lang.c.obj.ast.OrExpression;
import lang.c.obj.ast.PointerDereferenceExpression;
import lang.c.obj.ast.PostDecrementExpression;
import lang.c.obj.ast.PostIncrementExpression;
import lang.c.obj.ast.PreDecrementExpression;
import lang.c.obj.ast.PreIncrementExpression;
import lang.c.obj.ast.RShiftExpression;
import lang.c.obj.ast.Statement;
import lang.c.obj.ast.SubExpression;
import lang.c.obj.ast.TranslationUnit;
import lang.c.obj.ast.UnaryMinusExpression;
import lang.c.obj.ast.UnaryPlusExpression;
import lang.c.obj.ast.UnknownDeclaration;
import lang.c.obj.ast.UnknownStatement;


public class ClangASTTranslator implements ASTVisitor {
	private Map<ClangAST.Node, ASTNode> nodeMap = new HashMap<>();

	private void t(ClangAST.Node node, ASTNode internalNode) {
		nodeMap.put(node, internalNode);
	}

	private <T> T t(ClangAST.Node node) {
		return (T) nodeMap.get(node);
	}

	private <T extends ASTNode> Opt<T> opt(ClangAST.Node node) {
		if (node == null)
			return new Opt<T>();
		ASTNode n = (T) nodeMap.get(node);
		assert n != null;
		return new Opt(n);
	}

	private Statement stmt(ASTNode n) {
		if (n instanceof Expression) {
			return new ExpressionStatement((Expression) n);
		} else {
			return (Statement) n;
		}
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
		case "^" : c = BitwiseXorExpression::new; break;
		case "|" : c = BitwiseOrExpression::new; break;
		case "&&" : c = AndExpression::new; break;
		case "||" : c = OrExpression::new; break;
		case "=" : c = AssignmentExpression::new; break;
		default: throw new RuntimeException("Opcode not yet supported: " + b.opcode);
		}

		t(b, c.apply(lhs, rhs));
	}

	@Override public void visit(CompoundAssignOperator b) {
		Expression lhs = t(b.getLHS());
		Expression rhs = t(b.getRHS());

		BiFunction<Expression, Expression, Expression> c = null;

		switch (b.opcode) {
		case "+=": c = AssignmentAddExpression::new; break;
		case "-=": c = AssignmentSubExpression::new; break;
		case "*=": c = AssignmentMulExpression::new; break;
		case "/=": c = AssignmentDivExpression::new; break;
		case "%=": c = AssignmentModExpression::new; break;
		case ">>=": c = AssignmentRShiftExpression::new; break;
		case "<<=": c = AssignmentLShiftExpression::new; break;
		case "^=": c = AssignmentXorExpression::new; break;
		case "&=" : c = AssignmentAndExpression::new; break;
		case "|=" : c = AssignmentOrExpression::new; break;
		default: throw new RuntimeException("Opcode not yet supported: " + b.opcode);
		}

		t(b, c.apply(lhs, rhs));
	}
	@Override public void visit(ConditionalOperator c) { }
	@Override public void visit(UnaryOperator u) {
		Expression opd = t(u.getOperand());
		Function<Expression, Expression> c = null;

		switch (u.opcode) {
		case "++": c = u.isPostfix ? PostIncrementExpression::new : PreIncrementExpression::new; break;
		case "--": c = u.isPostfix ? PostDecrementExpression::new : PreDecrementExpression::new; break;
		case "&": c = AddressOfExpression::new; break;
		case "*": c = PointerDereferenceExpression::new; break;
		case "+": c = UnaryPlusExpression::new; break;
		case "-": c = UnaryMinusExpression::new; break;
		case "!": c = NotExpression::new; break;
		case "~": c = BitwiseNotExpression::new; break;
		default: throw new RuntimeException("Opcode not yet supported: " + u.opcode);
		}

		t(u, c.apply(opd));
	}
	@Override public void visit(DeclRefExpr e) {
		t(e, new IdentifierExpression(new Identifier(e.getDecl().name)));
	}

	@Override public void visit(ExplicitCastExpr e) {
	}

	@Override public void visit(ImplicitCastExpr e) {
		Expression operand = t(e.getOperand());
		t(e, operand);
	}

	@Override public void visit(IntegerLiteral n) {
		t(n, new ConstantExpression(new Constant()));
	}

	@Override public void visit(Stmt s) {
		List<ASTNode> children = new List<>();
		for (Node c : s.children()) {
			if (c != null) {
				ASTNode tc = t(c);
				if (tc != null)
					children.add(tc);
			}
		}
		t(s, new UnknownStatement(children));
	}

	@Override public void visit(ForStmt f) {
		ASTNode init = t(f.getInit());
		Opt<Expression> cond = opt(f.getCond());
		Opt<Expression> incr = opt(f.getIncr());
		Statement body = t(f.getBody());

		if (init == null) {
			t(f, new ForStatement(new Opt(), cond, incr, body));
		} else if (init instanceof Expression) {
			t(f, new ForStatement(new Opt(init), cond, incr, body));
		} else {
			// t(f, new ForDeclStatement((DeclarationStatement) init, cond, incr, body));
			visit((Stmt)f);
		}
	}
	@Override public void visit(CompoundStmt c) {
		List<Statement> stmts = new List<>();
		for (int i = 0; i < c.getNumStmts(); ++i) {
			stmts.add(stmt(t(c.getStmt(i))));
		}
		t(c, new CompoundStatement(stmts));
	}

	@Override public void visit(ReturnStmt r) {
		visit((Stmt) r);
	}

	@Override public void visit(DeclStmt r) {
		visit((Stmt) r);
	}

	@Override public void visit(Decl d) {
		List<ASTNode> children = new List<>();
		for (Node c : d.children()) {
			if (c != null) {
				ASTNode tc = t(c);
				if (tc != null)
					children.add(tc);
			}
		}

		t(d, new UnknownDeclaration(new List(), new List(), children));
	}

	@Override public void visit(ParmVarDecl p) {
		visit((Decl) p);
	}

	@Override public void visit(FunctionDecl f) {
		visit((Decl) f);
	}

	@Override public void visit(VarDecl v) {
		Expr init = v.getInit();
		if (init != null) {
			t(v, t(init));
		}
	}

	@Override public void visit(TranslationUnitDecl tu) {
		List<ExternalDeclarationOrDefinition> declOrDef = new List<>();
		for (int i = 0; i < tu.getNumDecl(); ++i) {
			ASTNode tn = t(tu.getDecl(i));
			if (tn instanceof Declaration) {
				declOrDef.add(new ExternalDeclaration((Declaration) tn));
			} else {
				declOrDef.add((FunctionDefinition) tn);
			}
		}
		t(tu, new TranslationUnit(declOrDef));
	}

	public ASTNode translate(ClangAST.Node root) {
		nodeMap.clear();
		root.acceptPO(this);
		return nodeMap.get(root);
	}
}
