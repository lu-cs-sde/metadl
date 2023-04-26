package clang;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.function.LongUnaryOperator;

import org.apache.commons.collections4.SetUtils;

import clang.swig.ClangClog;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;
import eval.Control;
import eval.EvaluationContext;
import eval.Operation;
import eval.Tuple;
import lang.ast.ExternalLiteral;
import lang.cons.ObjLangASTNode;


public class ClangEvaluationContext extends EvaluationContext {
  private ClangClogBuilder builder;
  private ClangClog clog;

  public ClangEvaluationContext(Set<String> srcs) {
    System.loadLibrary("clangClogSWIG");

    VectorString files = new VectorString(srcs);

    builder = new ClangClogBuilder(files);
    clog = builder.build();

    if (!clog.init()) {
      throw new RuntimeException("Error initializing clang-clog");
    }
  }

  public List<MatcherBuilder> genMatcherBuilder(lang.c.pat.ast.ASTNode internalNode) {
    Iterable<? extends AST.Node> clangNodes;

    if (internalNode instanceof lang.c.pat.ast.Declaration) {
      clangNodes = ((lang.c.pat.ast.Declaration) internalNode).clangDecls();
    } else if (internalNode instanceof lang.c.pat.ast.Statement) {
      clangNodes = List.of(((lang.c.pat.ast.Statement) internalNode).asClangStmt());
    } else if (internalNode instanceof lang.c.pat.ast.FunctionDefinition) {
      clangNodes = List.of(((lang.c.pat.ast.FunctionDefinition) internalNode).asClangDecl());
    } else {
      assert(internalNode instanceof lang.c.pat.ast.Expression);
      clangNodes = List.of(((lang.c.pat.ast.Expression) internalNode).asClangExpr());
    }

    List<MatcherBuilder> ret = new ArrayList<>();
    for (AST.Node clangNode : clangNodes) {
      ASTMatcherGen gen = new ASTMatcherGen();
      clangNode.acceptPO(gen);
      ret.add(gen.lookup(clangNode));
    }

    return ret;
  }

  public static class MatcherInfo {
    public long matcherId;
    public int[] resultMap;
    public String matcher;

    public static MatcherInfo of(String matcher, long matcherId, SortedSet<String> usedMetavars, Map<String, Integer> varToIdMap) {
      var r = new MatcherInfo();
      r.matcherId = matcherId;
      r.resultMap = new int[usedMetavars.size()];
      r.matcher = matcher;
      int i = 0;
      for (String v : usedMetavars) {
        r.resultMap[i++] = varToIdMap.get(v);
      }
      return r;
    }
  }

  public static Control genCodeExternalLiteral(ExternalLiteral l, Map<String, Integer> varToIdMap,
                                               ClangEvaluationContext clangCtx, Control cont) {

    List<List<MatcherInfo>> matcherIds = clangCtx.registerExternalLiteral(l, varToIdMap);
    java.util.List<Control> seq = new ArrayList<>();
    for (java.util.List<MatcherInfo> andMatchers : matcherIds) {
      Control next = cont;
      for (MatcherInfo m : andMatchers) {
        next = new ExternalForAll(clangCtx, m, next);
      }
      seq.add(next);
    }

    return Control.sequence(seq);
  }

  private List<List<MatcherInfo>> registerExternalLiteral(ExternalLiteral l, Map<String, Integer> varToIdMap) {
    // Each pattern can be parsed multiple ways (outer List)
    // Each parse of the pattern can yield multiple matches (inner List)
    // For a pattern to match as least one of its parses must match (OR outer list)
    // For a parse to match, all the generated clang matchers must match (AND inner list)
    List<List<MatcherInfo>> res = new ArrayList<>();

    ExternalLiteralPayload payload = (ExternalLiteralPayload) l.getExternalPayload();
    List<ObjLangASTNode> ASTs = payload.patterns;

    for (ObjLangASTNode Node : ASTs) {
      lang.c.pat.ast.ASTNode cNode = (lang.c.pat.ast.ASTNode) Node;
      Node.debugPrint(System.out);

      // One node may produce multiple matchers. For example
      // <: struct S { int x; } s; :> produces  a varDecl(...)
      // and a recordDecl(...) match
      Iterator<MatcherBuilder> mit = genMatcherBuilder(cNode).iterator();
      List<MatcherInfo> matchers = new ArrayList<>();

      Set<String> usedMetaVarSet = new TreeSet<>();
      Set<String> rootVariableSet = payload.rootVariable.isPresent() ?
        Collections.singleton(payload.rootVariable.get()) : Collections.emptySet();


      while (mit.hasNext()) {
        MatcherBuilder mb = mit.next();
        if (!mit.hasNext()) {
          // ASSUME: if the pattern has a root variable, attach it to last
          // matcher in the list (convention)
          // TODO: decide if root variables are needed at all in Clog
          payload.rootVariable.ifPresent(s -> mb.bind(s));
        }

        String matcher = mb.generate();
        long matcherId = clog.registerMatcher(matcher, true);

        if (matcherId < 0) {
          System.err.println("Failed to register matcher " + matcher);
          throw new RuntimeException();
        }

        matchers.add(MatcherInfo.of(matcher, matcherId ,mb.bindings(), varToIdMap));

        if (SetUtils.intersection(usedMetaVarSet, mb.bindings()).isEmpty()) {
          usedMetaVarSet.addAll(mb.bindings());
        } else {
          throw new RuntimeException("Metavariable used in two distinct clang patterns.");
        }
      }

      if (!SetUtils.union(rootVariableSet, Node.metavariables()).equals(usedMetaVarSet)) {
        throw new RuntimeException("Missing metavariables between the Clog and Clang patterns. ");
      }

      res.add(matchers);
    }

    return res;
  }

  private static abstract class SrcLocOperation implements Operation {
    private Operation nodeArg;
    private ClangEvaluationContext ctx;
    private String name;

    public SrcLocOperation(ClangEvaluationContext ctx, String name, Operation nodeArg) {
      this.nodeArg = nodeArg;
      this.ctx = ctx;
      this.name = name;
    }

    @Override public long eval(Tuple t) {
      long nid = nodeArg.eval(t);
      ClangClog.Loc loc = ctx.clog.srcLocation(nid);
      return processLoc(loc);
    }

    @Override public String prettyPrint() {
      return name + "(" + nodeArg.prettyPrint() + ")";
    }

    public abstract long processLoc(ClangClog.Loc loc);
  }

  private Operation makeUnaryOperation(String name, Operation arg0, LongUnaryOperator f) {
    return new Operation() {
      @Override public long eval(Tuple t) {
        long nid = arg0.eval(t);
        return f.applyAsLong(nid);
      }

      @Override public String prettyPrint() {
        return name + "(" + arg0.prettyPrint() + ")";
      }
    };
  }

  public Operation genExternalOperation(String name, List<Operation> args) {
    switch (name) {
    case "c_src_file":
      return new SrcLocOperation(this, name, args.get(0)) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return internalizeString(loc.getFilename());
        }
      };

    case "c_src_line_start":
      return new SrcLocOperation(this, name, args.get(0)) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartLine();
        }
      };

    case "c_src_line_end":
      return new SrcLocOperation(this, name, args.get(0)) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getEndLine();
        }
      };

    case "c_src_col_start":
      return new SrcLocOperation(this, name, args.get(0)) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartCol();
        }
      };

    case "c_src_col_end":
      return new SrcLocOperation(this, name, args.get(0)) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getEndCol();
        }
      };

    case "c_type":
      return makeUnaryOperation(name, args.get(0),
                                nid -> clog.type(nid));
    case "c_decl":
      return makeUnaryOperation(name, args.get(0),
                                nid -> clog.decl(nid));

    default:
      throw new RuntimeException("Unknown external operation '" + name + "'.");
    }
  }

  private boolean globalMatchersDone = false;

  public VectorVectorLong lookup(long matcherId) {
    if (!globalMatchersDone) {
      synchronized (this) {
        if (!globalMatchersDone) {
          clog.runGlobalMatchers();
          globalMatchersDone = true;
        }
      }
    }

    VectorVectorLong matches = clog.matchFromRoot(matcherId);
    return matches;
  }

  public enum ExternalLiteralKind {
    PATTERN
  }

  public static class ExternalLiteralPayload {
    List<ObjLangASTNode> patterns;
    Optional<String> rootVariable;
    ExternalLiteralKind kind;

    public static ExternalLiteralPayload pattern(List<ObjLangASTNode> patterns, Optional<String> rootVariable) {
      var ret =  new ExternalLiteralPayload();
      ret.kind = ExternalLiteralKind.PATTERN;
      ret.patterns = patterns;
      ret.rootVariable = rootVariable;
      return ret;
    }
  }
}
