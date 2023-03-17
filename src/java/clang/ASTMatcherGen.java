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

  @Override public void visit(AST.VarDecl n) {
    MatcherBuilder b = match("varDecl");
    if (n.isNamed()) {
      b.add(match("hasName", cst(n.getName())));
    }

    if (n.explicitType != null) {
      b.add(match("hasType", lookup(n.explicitType)));
    }

    if (n.bindsMetaVar()) {
      b.bind(n.boundMetaVar().getName());
    }

    matcherMap.put(n, b);
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

    if (n.bindsMetaVar()) {
      b.bind(n.boundMetaVar().getName());
    }

    matcherMap.put(n, b);
  }

  @Override public void visit(AST.PointerType p) {
    MatcherBuilder b = match("qualType");
    b.add(match("pointsTo", lookup(p.getPointeeType())));
    processTypeQualifiers(p, b);
    matcherMap.put(p, b);
  }

  @Override public void visit(AST.ArrayType a) {
    MatcherBuilder b = match("arrayType");
    b.add(match("hasElementType", lookup(a.getElementType())));
    processTypeQualifiers(a, b);
    matcherMap.put(a, b);
  }
}
