package clang;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static clang.MatcherBuilder.*;

class MatcherException extends RuntimeException {
  public MatcherException(String s) {
    super(s);
  }
}

public class ASTMatcherGen implements ASTVisitor {
  HashMap<AST.Node, MatcherBuilder> matcherMap = new HashMap<>();

  @Override public void visit(AST.Node n) {
    // do nothing
    System.err.println("Unhandled node kind: " + n.getClass().getSimpleName());
  }

  public MatcherBuilder lookup(AST.Node n) {
    MatcherBuilder b =  matcherMap.get(n);
    if (b == null) {
      n.acceptPO(this);
      b = matcherMap.get(n);
      if (b == null) {
        throw new RuntimeException("Node should have been visited.");
      }
    }

    return b;
  }

  public static void buildBindings(AST.Node n, MatcherBuilder b) {
    if (n.bindsMetaVar())
      b.bind(n.boundMetaVar().getName());
  }

  @Override public void visit(AST.VarDecl n) {
    MatcherBuilder b = match("varDecl");
    if (n.isNamed()) {
      b.add(match("hasName", cst(n.getName())));
    }

    if (n.explicitType != null) {
      b.add(match("hasType", lookup(n.explicitType)));
    }

    buildBindings(n, b);

    matcherMap.put(n, b);
  }

  @Override public void visit(AST.ParmVarDecl p) {
    MatcherBuilder b = match("parmVarDecl");
    if (p.isNamed())
      b.add(match("hasName", cst(p.getName())));

    if (p.explicitType != null) {
      b.add(match("hasType", lookup(p.explicitType)));
    }

    if (p.bindsMetaVar()) {
      b.bind(p.boundMetaVar().getName());
    }

    matcherMap.put(p, b);
  }

  public static void processTypeQualifiers(AST.Type t, MatcherBuilder b) {
    for (AST.TypeQualifier q : t.typeQuals) {
      switch (q) {
      case CONST:
        b.add(match("isConstQualified"));
        break;
      case VOLATILE:
        b.add(match("isVolatileQualified"));
        break;
      default:
        throw new MatcherException("Unsupported qualifier " + q + ".");
      }
    }
  }

  @Override public void visit(AST.BuiltinType n) {
    MatcherBuilder b = match("qualType");

    for (AST.BuiltinTypeKind kind : n.kinds) {
      switch (kind) {
      case CHAR:
        b.add(match("isAnyCharacter"));
        break;
      case SIGNED:
        b.add(match("isSignedInteger"));
        break;
      case UNSIGNED:
        b.add(match("isUnsignedInteger"));
        break;
      case INT:
        b.add(match("isInteger"));
        break;
      default:
        throw new MatcherException("Unsupported builtin type " + kind + ".");
      }
    }

    processTypeQualifiers(n, b);

    buildBindings(n, b);

    matcherMap.put(n, b);
  }

  @Override public void visit(AST.PointerType p) {
    MatcherBuilder b = match("qualType");
    b.add(match("pointsTo", lookup(p.getPointeeType())));
    processTypeQualifiers(p, b);

    buildBindings(p, b);

    matcherMap.put(p, b);
  }

  @Override public void visit(AST.ArrayType a) {
    MatcherBuilder b = match("arrayType");
    b.add(match("hasElementType", lookup(a.getElementType())));
    processTypeQualifiers(a, b);

    buildBindings(a, b);

    matcherMap.put(a, b);
  }

  @Override public void visit(AST.FunctionProtoType p) {
    MatcherBuilder b = match("functionProtoType");
    b.add(match("parameterCountIs", integer(p.getNumParamType())));
    for (int i = 0; i < p.getNumParamType(); ++i) {
      AST.Type pt = p.getParamType(i);
      b.add(match("hasParameterType", integer(i), lookup(pt)));
    }
    b.add(match("hasReturnType", lookup(p.getReturnType())));

    buildBindings(p, b);

    matcherMap.put(p, match("ignoringParens", b));
  }

  @Override public void visit(AST.Expr e) {
    MatcherBuilder b = match("expr");

    buildBindings(e, b);

    matcherMap.put(e, b);
  }

  @Override public void visit(AST.Stmt s) {
    MatcherBuilder b = match("stmt");

    buildBindings(s, b);

    matcherMap.put(s, b);
  }

  @Override public void visit(AST.WhileStmt s) {
    MatcherBuilder b = match("whileStmt");
    b.add(match("hasCondition", lookup(s.getCond())));
    b.add(match("hasBody", lookup(s.getBody())));

    buildBindings(s, b);

    matcherMap.put(s, b);
  }

  @Override public void visit(AST.DoStmt s) {
    MatcherBuilder b = match("doStmt");
    b.add(match("hasCondition", lookup(s.getCond())));
    b.add(match("hasBody", lookup(s.getBody())));

    buildBindings(s, b);

    matcherMap.put(s, b);
  }

  @Override public void visit(AST.ForStmt s) {
    MatcherBuilder b = match("forStmt");

    s.getInit().ifPresent(i -> b.add(match("hasLoopInit", lookup(i))));
    s.getCond().ifPresent(c -> b.add(match("hasCondition", lookup(c))));
    s.getIncr().ifPresent(i -> b.add(match("hasIncrement", lookup(i))));
    b.add(match("hasBody", lookup(s.getBody())));

    buildBindings(s, b);

    matcherMap.put(s, b);
  }
}
