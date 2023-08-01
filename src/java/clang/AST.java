package clang;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import lang.cons.ObjLangASTNode;

public class AST {
  public static class TextualType {
    public String qualType = "";
  }

  public static class MetaVar {
    private String name;
    public MetaVar(String name) {
      this.name = name;
    }
    public String getName() {
      return name;
    }
    public boolean isWildcard() {
      return name.equals("$_");
    }
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


    // bound metavars
    private MetaVar mv = null;
    public boolean bindsMetaVar() {
      return mv != null;
    }

    public MetaVar boundMetaVar() {
      return mv;
    }

    private <T extends Node> T setBinding(MetaVar mv) {
      if (this.mv != null)
        throw new IllegalStateException("This node already binds a metavariable.");
      if (this.isGap)
        throw new IllegalStateException("This node is a gap and can't bind a metavariable.");
      this.mv = mv;
      return (T) this;
    }

    private <T extends Node> T setBinding(String name) {
      return setBinding(new MetaVar(name));
    }

    public <T extends Node> T trySetBinding(ObjLangASTNode n) {
      if (!n.isWildcardNode())
        return setBinding(n.varName());
      return (T) this;
    }

    // Gaps
    private boolean isGap = false;
    public boolean isGap() {
      return isGap;
    }
    public <T extends Node> T gap() {
      if (this.mv != null)
        throw new IllegalStateException("This node already binds a metavariable.");
      this.isGap = true;
      return (T) this;
    }

    public void prettyPrint(PrintStream ps) {
      prettyPrintInternal(0, ps);
    }

    public java.util.List<Node> children() {
      if (inner == null)
        return Collections.emptyList();
      return Arrays.asList(inner);
    }

    public <T> T getChild(int i) {
      return (T) inner[i];
    }

    private void prettyPrintInternal(int offset, PrintStream ps) {
      for (int i = 0; i < offset; ++i) {
        ps.print("  ");
      }

      if (bindsMetaVar()) {
        ps.print(boundMetaVar().getName() + " := ");
      }

      if (isGap()) {
        ps.print("... ");
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

    protected <T extends Node> T setChildren(Collection<? extends Node> children) {
      this.inner = children.toArray(new Node[0]);
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

    public static Expr build() {
      return new Expr().setChildren();
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

    public static CallExpr build(Expr callee, List<Expr> args) {
      CallExpr ret = new CallExpr().setChildren();
      ret.inner = new Node[args.size() + 1];
      ret.inner[0] = callee;
      for (int i = 0; i < args.size(); ++i) {
        ret.inner[i + 1] = args.get(i);
      }
      return ret;
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

    public static BinaryOperator build(String op, Expr left, Expr right) {
      BinaryOperator b = new BinaryOperator().setChildren(left, right);
      b.opcode = op;
      return b;
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

    public static UnaryOperator build(String opcode, boolean isPostfix, Expr op) {
      UnaryOperator res = new UnaryOperator().setChildren(op);
      res.opcode = opcode;
      res.isPostfix = false;
      return res;
    }
  }

  public static class DeclRefExpr extends Expr {
    public Decl referencedDecl;

    public Decl getDecl() {
      return (Decl) referencedDecl;
    }

    String name;

    public boolean isNamed() {
      return name != null;
    }

    public String getName() {
      return name;
    }

    public <T extends Expr> T setName(String n) {
      if (name != null)
        throw new IllegalStateException("Decl already has a name.");
      this.name = n;
      return (T) this;
    }

    public static DeclRefExpr build() {
      return new DeclRefExpr().setChildren();
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

    public static ArraySubscriptExpr build(Expr lhs, Expr rhs) {
      return new ArraySubscriptExpr().setChildren(lhs, rhs);
    }
  }

  public static class CStyleCastExpr extends ExplicitCastExpr {
    Type destType;

    public static CStyleCastExpr build(Expr op, Type destType) {
      CStyleCastExpr r = new CStyleCastExpr().setChildren(op);
      r.destType = destType;
      return r;
    }

    public Type getDestType() {
      return destType;
    }

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }
  }

  public static class MemberExpr extends Expr {
    boolean isArrow;

    public static MemberExpr build(Expr base, FieldDecl member, boolean isArrow) {
      MemberExpr ret = new MemberExpr().setChildren(base, member);
      ret.isArrow = isArrow;
      return ret;
    }

    public Expr getBase() {
      return getChild(0);
    }

    public FieldDecl getMember() {
      return getChild(1);
    }

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
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

    public static Stmt build() {
      return new Stmt().setChildren();
    }
  }

  public static class ForStmt extends Stmt {
    public Optional<Stmt> getInit() {
      return Optional.ofNullable(getChild(0));
    }

    public Optional<Expr> getCond() {
      return Optional.ofNullable(getChild(2));
    }

    public Optional<Expr> getIncr() {
      return Optional.ofNullable(getChild(3));
    }

    public Stmt getBody() {
      return getChild(4);
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
    public Optional<Expr> getRetValue() {
      return (inner != null && inner.length != 0) ? Optional.of(getChild(0)) : Optional.empty();
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

    public static DeclStmt build(Decl ... decls) {
      return new DeclStmt().setChildren(decls);
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

    public static IfStmt build(Expr cond, Stmt thenBr) {
      IfStmt ret = new IfStmt().setChildren(cond, thenBr);
      ret.hasElse = false;
      return ret;
    }

    public static IfStmt build(Expr cond, Stmt thenBr, Stmt elseBr) {
      IfStmt ret = new IfStmt().setChildren(cond, thenBr, elseBr);
      ret.hasElse = true;
      return ret;
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

    public static DoStmt build(Expr cond, Stmt body) {
      return new DoStmt().setChildren(body, cond);
    }
  }

  public static class SwitchStmt extends Stmt {
    public Stmt getStmt() {
      return (Stmt) inner[1];
    }

    public Expr getExpr() {
      return (Expr) inner[0];
    }

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static SwitchStmt build(Expr expr, Stmt body) {
      return new SwitchStmt().setChildren(expr, body);
    }
  }

  public static class CaseStmt extends Stmt {
    public Stmt getStmt() {
      return (Stmt) inner[1];
    }

    public Expr getCaseConst() {
      return (Expr) inner[0];
    }

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static CaseStmt build(Expr expr, Stmt body) {
      return new CaseStmt().setChildren(expr, body);
    }
  }

  public static class DefaultStmt extends Stmt {
    public Stmt getStmt() {
      return getChild(0);
    }

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static DefaultStmt build(Stmt s) {
      return new DefaultStmt().setChildren(s);
    }
  }

  public static class BreakStmt extends Stmt {
    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static BreakStmt build() {
      return new BreakStmt().setChildren();
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
    public String name;
    public TextualType type;
    public Type explicitType;

    public boolean isNamed() {
      return name != null;
    }

    public String getName() {
      return name;
    }

    public <T extends Decl> T setName(String n) {
      if (name != null)
        throw new IllegalStateException("Decl already has a name.");
      this.name = n;
      return (T) this;
    }

    public static Decl build() {
      return new Decl().setChildren();
    }

    @Override protected String extraInfo() {
      String ret = "'" + (isNamed() ? getName() : "") + "' : ";
      if (explicitType != null) {
        ret += explicitType.printAsType();
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

    public static ParmVarDecl build() {
      return new ParmVarDecl().setChildren();
    }

  }

  public static class FunctionDecl extends Decl {
    public String mangledName;
    public boolean variadic;

    @Override protected String extraInfo() {
      return String.format(" mangledName:'%s' ", isNamed() ? getName() : "") + super.extraInfo();
    }

    @Override public boolean isNamed() {
      return mangledName != null;
    }

    @Override public FunctionDecl setName(String n) {
      mangledName = n;
      return this;
    }

    @Override public String getName() {
      return mangledName;
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

    public static FunctionDecl build(Type t, List<ParmVarDecl> params, boolean variadic) {
      FunctionDecl ret = new FunctionDecl();
      ret.explicitType = t;
      ret.variadic = variadic;
      return ret.setChildren(params.toArray(new Node[0]));
    }

    public static FunctionDecl build(String name, Type t, List<ParmVarDecl> params, Stmt body, boolean variadic) {
      FunctionDecl ret = new FunctionDecl();
      ret.explicitType = t;
      ret.variadic = variadic;
      return ret.setChildren(Stream.concat(params.stream(), List.of(body).stream()).toArray(Node[]::new));
    }

    public FunctionDecl setBody(Stmt body) {
      if (getBody() != null)
        throw new IllegalStateException("FunctionDecl already has a body");

      AST.Node[] newChildren = Arrays.copyOf(this.inner, this.inner.length + 1);
      newChildren[newChildren.length - 1] = body;
      return this.setChildren(newChildren);
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

    public VarDecl setInit(Expr init) {
      this.init = "c";
      setChildren(init);
      return this;
    }

    public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static VarDecl build(Type t) {
      VarDecl ret = new VarDecl();
      ret.explicitType = t;
      return ret.setChildren();
    }

    public static VarDecl build(Type t, Expr init) {
      return build(t).setChildren(init);
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

    public static FieldDecl build(Type t) {
      FieldDecl ret = new FieldDecl().setChildren();
      ret.explicitType = t;
      return ret;
    }

    public static FieldDecl build() {
      return new FieldDecl().setChildren();
    }

    public static FieldDecl build(Type t, Expr bitfieldSize) {
      FieldDecl ret = new FieldDecl().setChildren(bitfieldSize);
      ret.explicitType = t;
      return ret;
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


    public static RecordDecl build(String name, RecordKind kind, List<Decl> fields) {
      // NOTE: 'fields' can contain other type decls, comments, not only actual fields
      RecordDecl ret = new RecordDecl().setChildren(fields);
      ret.tagUsed = kind.getTag();
      ret.name = name;
      return ret;
    }

    public static RecordDecl build(RecordKind kind, List<Decl> fields) {
      // NOTE: 'fields' can contain other type decls, comments, not only actual fields
      RecordDecl ret = new RecordDecl().setChildren(fields);
      ret.tagUsed = kind.getTag();
      return ret;
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
      return printQuals();
    }

    protected String printQuals() {
      String ret = "";
      if (storage != StorageClass.AUTO) {
        ret += storage + " ";
      }

      for (TypeQualifier tq : typeQuals) {
        if (!ret.isEmpty())
          ret += " ";
        ret += tq;
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

    public String printAsType() {
      return "?";
    }
  }

  public static class AnonymousRecordType extends Type {
    // anonymous struct, union and enums
    RecordDecl decl;

    public static AnonymousRecordType build(RecordDecl decl) {
      AnonymousRecordType ret = new AnonymousRecordType().setChildren();
      ret.decl = decl;
      return ret;
    }

    @Override public String printAsType() {
      String quals = printQuals();
      if (!quals.isEmpty())
        quals = " " + quals;
      return "(" + RecordKind.fromTag(decl.tagUsed) + "?" + quals + ")";
    }
  }

  public static class TypedefType extends Type {
    String refName;

    public void accept(ASTVisitor v) {
      v.visit(this);
    }

    @Override public String printAsType() {
      String quals = printQuals();
      if (!quals.isEmpty())
        quals = " " + quals;
      return "(" + refName + quals + ")";
    }

    public static TypedefType build(String name) {
      TypedefType t = new TypedefType().setChildren();
      return t.setName(name);
    }

    public static TypedefType build() {
      return new TypedefType().setChildren();
    }

    public boolean isNamed() {
      return refName != null;
    }

    public String getName() {
      return refName;
    }

    public TypedefType setName(String name) {
      this.refName = name;
      return this;
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

    public String printAsType() {
      String quals = printQuals();
      if (!quals.isEmpty())
        quals += " ";
      return "(* " + quals + getPointeeType().printAsType() + ")";
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

    public String printAsType() {
      String quals = printQuals();
      if (!quals.isEmpty())
        quals += " ";
      return "([] "  + quals + getElementType().printAsType() + ")";
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
    public boolean variadic;

    public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public Type getReturnType() {
      if (inner != null && inner.length > 0) {
        return (Type) inner[0];
      }
      return null;
    }

    public int getNumParamType() {
      return Integer.max(inner.length - 1, 0);
    }

    public Type getParamType(int i) {
      return (Type) inner[i + 1];
    }

    public static FunctionProtoType build(Type retType, List<Type> args, boolean variadic) {
      FunctionProtoType ret = new FunctionProtoType().setChildren();

      ret.inner = new Node[args.size() + 1];
      ret.inner[0] = retType;
      ret.variadic = variadic;

      for (int i = 0; i < args.size(); ++i) {
        ret.inner[i + 1] = args.get(i);
      }
      return ret;
    }

    public String printAsType() {
      String ret = "(() " + getReturnType().printAsType();
      for (int i = 0; i < getNumParamType(); ++i) {
        ret += " " + getParamType(i).printAsType();
      }
      ret += ")";

      return ret;
    }
  }

  public static class FunctionNoProtoType extends Type {
    // Represent K&R style definitions
    // int f();
    public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public static FunctionNoProtoType build(Type retType) {
      return new FunctionNoProtoType().setChildren(retType);
    }

    public Type getReturnType() {
      return getChild(0);
    }
  }

  public static class GapType extends Type {
    public String printAsType() {
      return "..";
    }

    public static GapType build() {
      return new GapType().setChildren().gap();
    }
  }

  public enum RecordKind {
    STRUCT("struct"),
    UNION("union"),
    CLASS("class"),
    ENUM("enum");

    private String tag;
    private RecordKind(String tag) {
      this.tag = tag;
    }
    public String getTag() {
      return tag;
    }
    public static RecordKind fromTag(String tag) {
      switch (tag) {
      case "struct":
        return STRUCT;
      case "union":
        return UNION;
      case "class":
        return CLASS;
      default:
        throw new IllegalArgumentException();
      }
    }
  }

  public static class RecordRefType extends Type {
    RecordKind kind;
    String name;

    public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public String printAsType() {
      String quals = printQuals();
      if (!quals.isEmpty())
        quals = " " + quals;
      return "(" + kind.toString() + " " + name + quals + ")";
    }

    public static RecordRefType build(RecordKind kind, String name) {
      RecordRefType ref = new RecordRefType().setChildren();
      ref.kind = kind;
      ref.name = name;
      return ref;
    }

    public boolean isNamed() {
      return name != null;
    }

    public String getName() {
      return name;
    }

    public RecordRefType setName(String name) {
      this.name = name;
      return this;
    }

    public static RecordRefType build(RecordKind kind) {
      RecordRefType ref = new RecordRefType().setChildren();
      ref.kind = kind;
      return ref;
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

    @Override public void accept(ASTVisitor v) {
      v.visit(this);
    }

    public boolean isVoid() {
      return kinds.contains(BuiltinTypeKind.VOID);
    }

    @Override public String extraInfo() {
      String ret = super.extraInfo();
      for (BuiltinTypeKind kind : kinds) {
        ret += " " + kind ;
      }
      return ret;
    }

    @Override public String printAsType() {
      String ret = "";
      for (BuiltinTypeKind kind : kinds) {
        if (!ret.isEmpty())
          ret += " ";
        else
          ret += "(";
        ret += kind.toString();
      }
      String quals = printQuals();
      if (!quals.isEmpty())
        ret += " " + quals ;
      ret += ")";
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
}
