package clang;

import static clang.ASTTrans.list;
import static prof.Profile.profile;

import java.util.HashMap;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;

import clang.AST.ArraySubscriptExpr;
import clang.AST.ArrayType;
import clang.AST.BinaryOperator;
import clang.AST.BuiltinType;
import clang.AST.CXXForRangeStmt;
import clang.AST.CallExpr;
import clang.AST.Comment;
import clang.AST.CompoundAssignOperator;
import clang.AST.CompoundStmt;
import clang.AST.ConditionalOperator;
import clang.AST.Decl;
import clang.AST.DeclRefExpr;
import clang.AST.DeclStmt;
import clang.AST.DoStmt;
import clang.AST.ExplicitCastExpr;
import clang.AST.Expr;
import clang.AST.FieldDecl;
import clang.AST.ForStmt;
import clang.AST.FunctionDecl;
import clang.AST.FunctionProtoType;
import clang.AST.IfStmt;
import clang.AST.ImplicitCastExpr;
import clang.AST.IntegerLiteral;
import clang.AST.Node;
import clang.AST.ParenType;
import clang.AST.ParmVarDecl;
import clang.AST.PointerType;
import clang.AST.RecordDecl;
import clang.AST.ReturnStmt;
import clang.AST.Stmt;
import clang.AST.TranslationUnitDecl;
import clang.AST.Type;
import clang.AST.UnaryOperator;
import clang.AST.VarDecl;
import clang.AST.WhileStmt;
import lang.c.obj.ast.ASTNode;
import lang.c.obj.ast.AddExpression;
import lang.c.obj.ast.AddressOfExpression;
import lang.c.obj.ast.AndExpression;
import lang.c.obj.ast.ArrayIndexExpression;
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
import lang.c.obj.ast.CommaExpression;
import lang.c.obj.ast.CompoundStatement;
import lang.c.obj.ast.Constant;
import lang.c.obj.ast.ConstantExpression;
import lang.c.obj.ast.Declaration;
import lang.c.obj.ast.DeclarationOrDefinition;
import lang.c.obj.ast.DeclarationSpecifier;
import lang.c.obj.ast.DeclarationStatement;
import lang.c.obj.ast.Declarator;
import lang.c.obj.ast.DivExpression;
import lang.c.obj.ast.DoWhileStatement;
import lang.c.obj.ast.EQExpression;
import lang.c.obj.ast.Expression;
import lang.c.obj.ast.ForRangeStatement;
import lang.c.obj.ast.ForStatement;
import lang.c.obj.ast.FunctionDeclarator;
import lang.c.obj.ast.FunctionDefinition;
import lang.c.obj.ast.GEQExpression;
import lang.c.obj.ast.GTExpression;
import lang.c.obj.ast.Identifier;
import lang.c.obj.ast.IdentifierDeclarator;
import lang.c.obj.ast.IdentifierExpression;
import lang.c.obj.ast.IfElseStatement;
import lang.c.obj.ast.IfStatement;
import lang.c.obj.ast.InitDeclarator;
import lang.c.obj.ast.Initializer;
import lang.c.obj.ast.InitializerExpression;
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
import lang.c.obj.ast.ParameterDeclaration;
import lang.c.obj.ast.ParameterType;
import lang.c.obj.ast.ParameterVarArgType;
import lang.c.obj.ast.PointerDereferenceExpression;
import lang.c.obj.ast.PostDecrementExpression;
import lang.c.obj.ast.PostIncrementExpression;
import lang.c.obj.ast.PreDecrementExpression;
import lang.c.obj.ast.PreIncrementExpression;
import lang.c.obj.ast.RShiftExpression;
import lang.c.obj.ast.Register;
import lang.c.obj.ast.Statement;
import lang.c.obj.ast.Static;
import lang.c.obj.ast.StructDeclaration;
import lang.c.obj.ast.StructDeclarator;
import lang.c.obj.ast.StructSpecifier;
import lang.c.obj.ast.SubExpression;
import lang.c.obj.ast.TranslationUnit;
import lang.c.obj.ast.TypeSpecifier;
import lang.c.obj.ast.UnaryMinusExpression;
import lang.c.obj.ast.UnaryPlusExpression;
import lang.c.obj.ast.UnionSpecifier;
import lang.c.obj.ast.UnknownDeclaration;
import lang.c.obj.ast.UnknownExpression;
import lang.c.obj.ast.UnknownStatement;
import lang.c.obj.ast.UnknownTypeSpecifier;
import lang.c.obj.ast.WhileStatement;


public class ASTTranslator implements ASTVisitor {
	private Map<AST.Node, ASTNode> nodeMap = new HashMap<>();

	private void t(AST.Node node, ASTNode internalNode) {
		internalNode.setStart(node.range.begin.getLine(), node.range.begin.getCol());
		internalNode.setEnd(node.range.end.getLine(), node.range.end.getCol());
		internalNode.srcFile = node.range.begin.getFile();
		nodeMap.put(node, internalNode);
	}

	private <T> T t(AST.Node node) {
		return (T) nodeMap.get(node);
	}

	private <T extends ASTNode<ASTNode>> Opt<T> opt(AST.Node node) {
		if (node == null)
			return new Opt<T>();
		ASTNode n = (T) nodeMap.get(node);
		assert n != null;
		return new Opt(n);
	}

	private <T extends ASTNode<ASTNode>> List<T> singletonList(T n) {
		return new List<T>().add(n);
	}

	private List emptyList() {
		return new List();
	}

	private <T extends ASTNode<ASTNode>, U extends ASTNode<ASTNode>> Opt<T> opt(Opt<U> opt, Function<U, T> cons) {
		if (opt.getNumChild() == 0) {
			return new Opt<T>();
		} else {
			return new Opt<T>(cons.apply(opt.getChild(0)));
		}
	}

	@Override public void visit(Node n) { }

	@Override public void visit(Expr e) {
		List<ASTNode> children = new List<>();
		for (Node c : e.children()) {
			if (c != null) {
				ASTNode tc = t(c);
				if (tc != null)
					children.add(tc);
			}
		}
		t(e, new UnknownExpression(children));

		// SimpleLogger.logger().info("[clang-ast-translation] " + e.kind);
		profile().incCounter("clang_ast_nodes", e.kind);
	}

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
		case "," : c = CommaExpression::new; break;
		default:
			profile().incCounter("clang_ast_nodes", b.kind + b.opcode);
			visit((Expr) b);
			return;
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
		default:
			profile().incCounter("clang_ast_nodes", b.kind + b.opcode);
			visit((Expr) b);
			return;
		}

		t(b, c.apply(lhs, rhs));
	}

	@Override public void visit(ConditionalOperator c) {
		visit((Expr) c);
	}

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
		default:
			profile().incCounter("clang_ast_nodes", u.kind + u.opcode);
			visit((Expr) u);
			return;
		}

		t(u, c.apply(opd));
	}
	@Override public void visit(DeclRefExpr e) {
		t(e, new IdentifierExpression(new Identifier(e.getDecl().name)));
	}

	@Override public void visit(ExplicitCastExpr e) {
		visit((Expr) e);
	}

	@Override public void visit(ImplicitCastExpr e) {
		Expression operand = t(e.getOperand());
		t(e, operand);
	}

	@Override public void visit(IntegerLiteral n) {
		t(n, new ConstantExpression(new Constant()));
	}

	@Override public void visit(ArraySubscriptExpr a) {
		Expression lhs = t(a.getLHS());
		Expression rhs = t(a.getRHS());
		t(a, new ArrayIndexExpression(lhs, rhs));
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

		// SimpleLogger.logger().info("[clang-ast-translation] " + s.kind);
		profile().incCounter("clang_ast_nodes", s.kind);

		t(s, new UnknownStatement(children));
	}

	@Override public void visit(ForStmt f) {
		ASTNode init = t(f.getInit().orElse(null));
		Opt<Expression> cond = opt(f.getCond().orElse(null));
		Opt<Expression> incr = opt(f.getIncr().orElse(null));
		Statement body = t(f.getBody());

		if (init == null) {
			t(f, new ForStatement(new Opt(), cond, incr, body));
		} else if (init instanceof Statement) {
			t(f, new ForStatement(new Opt(init), cond, incr, body));
		} else {
			visit((Stmt) f);
		}
	}

	@Override public void visit(DoStmt d) {
		Expression cond = t(d.getCond());
		Statement body = t(d.getBody());
		t(d, new DoWhileStatement(cond, body));
	}

	@Override public void visit(WhileStmt w) {
		Expression cond = t(w.getCond());
		Statement body = t(w.getBody());
		t(w, new WhileStatement(cond, body));
	}

	@Override public void visit(CompoundStmt c) {
		List<Statement> stmts = new List<>();
		for (int i = 0; i < c.getNumStmts(); ++i) {
			stmts.add(t(c.getStmt(i)));
		}
		t(c, new CompoundStatement(stmts));
	}

	@Override public void visit(CXXForRangeStmt f) {
		// ForRangeStatement : Statement ::= [Init:Statement] Range:Declaration Body:Statement;x
		DeclarationStatement rangeStmt = t(f.getRangeStmt());
		Declaration rangeDecl = (Declaration) rangeStmt.getDeclarationOrDefinition();
		Statement body = t(f.getBody());
		t(f, new ForRangeStatement(new Opt<Statement>(), rangeDecl, body));
	}

	@Override public void visit(ReturnStmt r) {
		visit((Stmt) r);
	}

	@Override public void visit(DeclStmt r) {
		if (r.getNumDecl() == 1) {
			t(r, new DeclarationStatement(t(r.getDecl(0))));
		} else {
			visit((Stmt) r);
		}
	}

	@Override public void visit(IfStmt f) {
		Expression cond = t(f.getCond());
		Statement th = t(f.getThen());
		if (f.hasElse) {
			t(f, new IfElseStatement(cond, th, t(f.getElse())));
		} else {
			t(f, new IfStatement(cond, th));
		}
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


		profile().incCounter("clang_ast_nodes", d.kind);

		t(d, new UnknownDeclaration(new List(), new List(), children));

		// SimpleLogger.logger().info("[clang-ast-translation] " + d.kind);
	}

	@Override public void visit(ParmVarDecl p) {
		IdentifierDeclarator id = new IdentifierDeclarator(new Identifier(p.name));
		UnknownTypeSpecifier typeSpec = new UnknownTypeSpecifier(p.type.qualType);

		ParameterDeclaration decl = new ParameterDeclaration(new List().add(typeSpec), id);
		t(p, decl);
	}

	@Override public void visit(FunctionDecl f) {
		List<ParameterType> paramDecls = new List<>();
		for (ParmVarDecl p : f.getParams()) {
			paramDecls.add(t(p));
		}

		if (f.variadic) {
			paramDecls.add(new ParameterVarArgType());
		}

		IdentifierDeclarator id = new IdentifierDeclarator(new Identifier(f.name));
		FunctionDeclarator fd = new FunctionDeclarator(id, paramDecls);
		UnknownTypeSpecifier typeSpec = new UnknownTypeSpecifier(f.type.qualType);

		DeclarationOrDefinition ext;
		if (f.getBody() != null) {
			ext = new FunctionDefinition(new List<DeclarationSpecifier>().add(typeSpec), fd, new List<Declaration>(), t(f.getBody()));
		} else {
			ext = new Declaration(new List<DeclarationSpecifier>().add(typeSpec), new List<InitDeclarator>().add(new InitDeclarator(fd, new Opt<Initializer>())));
		}
		t(f, ext);
	}

	@Override public void visit(VarDecl v) {
		List<DeclarationSpecifier> scSpecs = new List<>();
		if (v.storageClass != null) {
			switch (v.storageClass) {
			case "static":
				scSpecs.add(new Static());
				break;
			case "register":
				scSpecs.add(new Register());
				break;
			default:
				// do nothing
				break;
			}
		}


		Opt<Expression> init = opt(v.getInit());
		Opt initExpr = opt(init, InitializerExpression::new);
		IdentifierDeclarator id = new IdentifierDeclarator(new Identifier(v.name));
		InitDeclarator initDecl = new InitDeclarator(id, initExpr);
		UnknownTypeSpecifier typeSpec = new UnknownTypeSpecifier(v.type.qualType);

		scSpecs.add(typeSpec);
		Declaration d = new Declaration(scSpecs, new List().add(initDecl));
		t(v, d);
	}

	@Override public void visit(FieldDecl v) {
		Opt<Declarator> id;
		if (v.name != null && v.name.length() > 0) {
			id = new Opt<>(new IdentifierDeclarator(new Identifier(v.name)));
		} else {
			// bitfield padding, e.g.  "int : 5;"
			id = new Opt<>();
		}

		Opt<Expression> bitfieldSize;
		if (v.isBitfield) {
			bitfieldSize = new Opt<>(t(v.getBitFieldSize()));
		} else {
			bitfieldSize = new Opt<>();
		}

		List<TypeSpecifier> typeSpec = new List<TypeSpecifier>().add(new UnknownTypeSpecifier(v.type.qualType));

		t(v, new StructDeclaration(typeSpec, new List<>(), new List<StructDeclarator>().add(new StructDeclarator(id, bitfieldSize))));
	}

	@Override public void visit(RecordDecl d) {
		List structDecls = new List();
		for (int i = 0; i < d.getNumDecl(); ++i) {
			if (d.getDecl(i) instanceof FieldDecl) {
				structDecls.add(t(d.getDecl(i)));
			} else if (d.getDecl(i) instanceof RecordDecl) {
				structDecls.add(new StructDeclaration(new List().add(t(d.getDecl(i))), new List(), new List()));
			} else {
				// bail out
				visit((Decl) d);
			}
		}

		Opt name = d.isNamed() ? new Opt(new Identifier(d.getName())) : new Opt();

		if (d.tagUsed.equals("struct")) {
			t(d, new Declaration(singletonList(new StructSpecifier(name, structDecls)), emptyList()));
		} else if (d.tagUsed.equals("union")) {
			t(d, new Declaration(singletonList(new UnionSpecifier(name, structDecls)), emptyList()));
		} else {
			// bail out
			visit((Decl) d);
		}
	}

	@Override public void visit(PointerType t) {

	}

	@Override public void visit(ParenType t) {

	}

	@Override public void visit(FunctionProtoType t) {

	}

	@Override public void visit(BuiltinType t) {

	}

	@Override public void visit(Type t) {

	}

	@Override public void visit(ArrayType t) {
	}

	@Override public void visit(Comment c) {
		t(c, new lang.c.obj.ast.Comment(new List(), new List(), c.text, new List()));
	}

	@Override public void visit(TranslationUnitDecl tu) {
		List<DeclarationOrDefinition> declOrDef = new List<>();
		for (int i = 0; i < tu.getNumDecl(); ++i) {
			if (tu.getDecl(i) == null) {
				// this occurs when skipping nodes from included files
				continue;
			}
			ASTNode tn = t(tu.getDecl(i));
			if (tn instanceof DeclarationOrDefinition) {
				declOrDef.add((DeclarationOrDefinition) tn);
			} else {
				throw new RuntimeException("Unexpected top-level member of TranslationUnit.");
			}
		}
		t(tu, new TranslationUnit(declOrDef));
	}

	public ASTNode translate(AST.Node root) {
		nodeMap.clear();
		root.acceptPO(this);
		return nodeMap.get(root);
	}

	// An embedded DSL for writing AST to AST translators.
	// Java is a rather verbose language to write a DSL in.
	public ASTTrans<Expr, Expression> Expr_Expression = (Expr e) -> (Expression) null;
	public ASTTrans<VarDecl, Declaration> VarDecl_Declaration = new ASTTrans<>() {
			@Override public Declaration trans(VarDecl v) {
				ASTTrans<VarDecl, List<DeclarationSpecifier>> scSpecs =
				ASTTrans.<VarDecl, String, List<DeclarationSpecifier>>match((VarDecl vd) -> vd.storageClass)
				.of(s -> s.equals("static"), list(Static::new))
				.of(s -> s.equals("register"), list(Register::new))
				.otherwise(list());

				ASTTrans<VarDecl, Opt<Expression>> init = ASTTrans.opt(Expr_Expression.after((VarDecl vd) -> vd.getInit()));
				return null;
			}
		};
}
