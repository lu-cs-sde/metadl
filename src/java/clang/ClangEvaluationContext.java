package clang;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.SortedSet;
import java.util.function.LongBinaryOperator;
import java.util.function.LongUnaryOperator;

import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.tuple.Pair;

import clang.swig.ClangClog;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;
import eval.Control;
import eval.EvaluationContext;
import eval.Operation;
import eval.Tuple;
import lang.ast.CodeGenContext;
import lang.ast.ExternalLiteral;
import lang.ast.Variable;
import lang.cons.ObjLangASTNode;


public class ClangEvaluationContext extends EvaluationContext {
  private ClangClogBuilder builder;
  private ClangClog clog;

  public ClangEvaluationContext(Set<String> srcs, List<String> clangArgs) {
    if (srcs.isEmpty())
      return;

    System.loadLibrary("clangClogSWIG");

    VectorString args = new VectorString(clangArgs);
    args.addAll(srcs);

    builder = new ClangClogBuilder(args);
    clog = builder.build();

    if (!clog.init()) {
      throw new RuntimeException("Error initializing clang-clog");
    }
  }

  public static List<MatcherBuilder> genMatcherBuilder(lang.c.pat.ast.ASTNode internalNode) {
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

  public static Control genCodeExternalLiteral(CodeGenContext cgx, ExternalLiteral l, Set<String> boundVars,
                                               ClangEvaluationContext clangCtx, Control cont) {

    ExternalLiteralPayload payload = (ExternalLiteralPayload) l.getExternalPayload();

    switch (payload.kind) {
    case PATTERN: {
      List<MatcherInfo> andMatchers = clangCtx.registerExternalLiteral(cgx, payload, boundVars);

      if (andMatchers.isEmpty())
        return Control.nop();

      Control next = cont;
      for (MatcherInfo m : andMatchers) {
        next = m.genControl(clangCtx, next);
      }

      return next;
    }
    case CFG_SUCC: {
      Variable nVar = (Variable) l.getTerms(0);
      Variable sVar = (Variable) l.getTerms(1);

      int nVarIdx = cgx.varIndex(nVar);
      int sVarIdx = cgx.varIndex(sVar);

      return new Control() {
        @Override public void eval(Tuple t) {
          long nid = t.get(nVarIdx);

          for (long res : clangCtx.clog.cfgSucc(nid)) {
            t.set(sVarIdx, res);
            cont.eval(t);
          }
        }

        @Override public String prettyPrint(int indent) {
          String s = Control.Util.indent(indent)
            + String.format("EXTERNAL FOR t IN %s WHERE ", payload.kind.name())
            + "\n" + cont.prettyPrint(indent + 1);
          return s;
        }
      };
    }

    case CFG_EXIT: {
      Variable nVar = (Variable) l.getTerms(0);
      Variable sVar = (Variable) l.getTerms(1);

      int nVarIdx = cgx.varIndex(nVar);
      int sVarIdx = cgx.varIndex(sVar);

      return new Control() {
        @Override public void eval(Tuple t) {
          long nid = t.get(nVarIdx);

          for (long res : clangCtx.clog.cfgExit(nid)) {
            t.set(sVarIdx, res);
            cont.eval(t);
          }
        }

        @Override public String prettyPrint(int indent) {
          String s = Control.Util.indent(indent)
            + String.format("EXTERNAL FOR t IN %s WHERE ", payload.kind.name())
            + "\n" + cont.prettyPrint(indent + 1);
          return s;
        }
      };
    }

    default:
    case PARENT:
      return Control.nop();
    }
  }

  private List<MatcherInfo> registerExternalLiteral(CodeGenContext cgx, ExternalLiteralPayload payload, Set<String> boundVars) {
    List<MatcherBuilder> matchers = analyzeExternalLiteral(payload);
    List<MatcherInfo> andMatchersOut = new ArrayList<>();

    for (MatcherBuilder andMatcher : matchers) {
      Map<String, Integer> varToIdMap = new HashMap<>();

      boolean needsGlobalMatcher = !(payload.subtreeVariable.isPresent() ||
          andMatcher.hasBinding() && boundVars.contains(andMatcher.getBinding()));


      // iterate through all the variables that occur inside the matcher and make
      // sure they are not bound
      for (String binding : andMatcher.bindings()) {
        if (!varToIdMap.containsKey(binding)) {
          int varIdx;
          if (boundVars.contains(binding) &&
              !(andMatcher.hasBinding() && andMatcher.getBinding().equals(binding))) {
            // this variable is bound by a previous literal, but it is not the root of the matcher,
            // rename it
            varIdx = cgx.freshIndex();
            // and add an equality constraint
            andMatchersOut.add(MatcherInfo.eq(varIdx, cgx.varIndex(binding)));
          } else {
            varIdx = cgx.varIndex(binding);
          }
          varToIdMap.put(binding, varIdx);
        }
      }


      String m = andMatcher.generate();


      long matcherId = -1;

      if (clog != null) {
        System.err.print("Registered '" + (needsGlobalMatcher ? "global" :
                                           (payload.subtreeVariable.isPresent() ? "fromNode" : "atNode")) + "' matcher " + m);

        matcherId = clog.registerMatcher(m, needsGlobalMatcher);
        if (matcherId < 0) {
          System.err.println("Failed to register matcher " + m);
          throw new RuntimeException();
        } else {
          System.err.println("[" + matcherId + "]");
        }
      }

      if (payload.subtreeVariable.isPresent()) {
        varToIdMap.put(payload.subtreeVariable.get(), cgx.varIndex(payload.subtreeVariable.get()));
      }

      if (needsGlobalMatcher) {
        andMatchersOut.add(MatcherInfo.global(m, matcherId, andMatcher.bindings(), varToIdMap));
      } else {
        if (payload.subtreeVariable.isPresent()) {
          if (!boundVars.contains(payload.subtreeVariable.get())) {
            System.err.println("Subtree variable '" + payload.subtreeVariable.get() + "' must be bound earlier in the clause.");
            throw new RuntimeException();
          }
          andMatchersOut.add(MatcherInfo.matchFromNode(m, matcherId, andMatcher.bindings(), varToIdMap, payload.subtreeVariable.get()));
        } else {
          andMatchersOut.add(MatcherInfo.matchAtNode(m, matcherId, andMatcher.bindings(), varToIdMap, andMatcher.getBinding()));
        }
      }
    }

    return andMatchersOut;
  }


  public static List<MatcherBuilder> analyzeExternalLiteral(ExternalLiteralPayload payload) {
    // Each parse of the pattern can yield multiple matchers (inner List)
    // For a pattern to match, all the generated clang matchers must match (AND inner list)
    lang.c.pat.ast.ASTNode cNode = (lang.c.pat.ast.ASTNode) payload.pattern;
    // Node.debugPrint(System.out);

    // One node may produce multiple matchers. For example
    // <: struct S { int x; } s; :> produces  a varDecl(...)
    // and a recordDecl(...) match
    List<MatcherBuilder> matchers = genMatcherBuilder(cNode);
    Iterator<MatcherBuilder> mit = matchers.iterator();

    Set<String> usedMetaVarSet = new TreeSet<>();
    Set<String> rootVariableSet = payload.rootVariable.isPresent() ?
      Collections.singleton(payload.rootVariable.get()) : Collections.emptySet();


    while (mit.hasNext()) {
      MatcherBuilder mb = mit.next();
      if (!mit.hasNext()) {
        // ASSUME: if the pattern has a root variable, attach it to last
        // matcher in the list (convention)
        // TODO: decide if root variables are needed at all in Clog
        if (payload.rootVariable.isPresent() &&
            mb.hasBinding()) {
          System.out.println("Skipping interpretation of pattern " +
                             mb.generate() + " due to double binding of root variable.");
          return Collections.emptyList();
        }
        payload.rootVariable.ifPresent(s -> mb.bind(s));
      }


      if (SetUtils.intersection(usedMetaVarSet, mb.bindings()).isEmpty()) {
        usedMetaVarSet.addAll(mb.bindings());
      } else {
        throw new RuntimeException("Metavariable used in two distinct clang patterns.");
      }
    }

    if (!SetUtils.union(rootVariableSet, cNode.metavariables()).equals(usedMetaVarSet)) {
      for (MatcherBuilder mb : matchers) {
        System.err.println(mb.generate());
      }
      throw new RuntimeException("Missing metavariables between the Clog and Clang patterns. ");
    }

    return matchers;
  }


  private static abstract class SrcLocOperation implements Operation {
    private final Operation nodeArg;
    private final ClangEvaluationContext ctx;
    private final String name;
    private final boolean expansion;

    public SrcLocOperation(ClangEvaluationContext ctx, String name, Operation nodeArg, boolean expansion) {
      this.nodeArg = nodeArg;
      this.ctx = ctx;
      this.name = name;
      this.expansion = expansion;
    }

    @Override public long eval(Tuple t) {
      long nid = nodeArg.eval(t);
      ClangClog.Loc loc = expansion ?
        ctx.clog.srcExpansionLocation(nid) :
        ctx.clog.srcLocation(nid);
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

  private Operation makeBinaryOperation(String name, Operation arg0, Operation arg1, LongBinaryOperator f) {
    return new Operation() {
      @Override public long eval(Tuple t) {
        long n0 = arg0.eval(t);
        long n1 = arg1.eval(t);
        return f.applyAsLong(n0, n1);
      }

      @Override public String prettyPrint() {
        return name + "(" + arg0.prettyPrint() + ")";
      }
    };
  }

  public Operation genExternalOperation(String name, List<Operation> args) {
    switch (name) {
    case "c_src_file":
      return new SrcLocOperation(this, name, args.get(0), false) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return internalizeString(loc.getFilename());
        }
      };

    case "c_src_line_start":
      return new SrcLocOperation(this, name, args.get(0), false) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartLine();
        }
      };

    case "c_src_line_end":
      return new SrcLocOperation(this, name, args.get(0), false) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getEndLine();
        }
      };

    case "c_src_col_start":
      return new SrcLocOperation(this, name, args.get(0), false) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartCol();
        }
      };

    case "c_src_col_end":
      return new SrcLocOperation(this, name, args.get(0), false) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getEndCol();
        }
      };


    case "c_src_exp_file":
      return new SrcLocOperation(this, name, args.get(0), true) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return internalizeString(loc.getFilename());
        }
      };

    case "c_src_exp_line_start":
      return new SrcLocOperation(this, name, args.get(0), true) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartLine();
        }
      };

    case "c_src_exp_line_end":
      return new SrcLocOperation(this, name, args.get(0), true) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getEndLine();
        }
      };

    case "c_src_exp_col_start":
      return new SrcLocOperation(this, name, args.get(0), true) {
        @Override public long processLoc(ClangClog.Loc loc) {
          return loc.getStartCol();
        }
      };

    case "c_src_exp_col_end":
      return new SrcLocOperation(this, name, args.get(0), true) {
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
    case "c_name":
      return makeUnaryOperation(name, args.get(0),
                                nid -> this.internalizeString(clog.name(nid)));
    case "c_is_parent":
      return makeBinaryOperation(name, args.get(0), args.get(1),
                                 (n0, n1) -> clog.isParent(n0, n1) ? 1 : 0);

    case "c_is_ancestor":
      return makeBinaryOperation(name, args.get(0), args.get(1),
                                 (n0, n1) -> clog.isAncestor(n0, n1) ? 1 : 0);

    case "c_parent":
      return makeUnaryOperation(name, args.get(0),
                                nid -> clog.parent(nid));

    case "c_enclosing_function":
      return makeUnaryOperation(name, args.get(0),
                                nid -> clog.enclosingFunction(nid));

    case "c_index":
      return makeUnaryOperation(name, args.get(0), nid -> clog.index(nid));

    case "c_has_global_storage":
      return makeUnaryOperation(name, args.get(0), nid -> clog.hasGlobalStorage(nid) ? 1 : 0);

    case "c_cfg":
      return makeUnaryOperation(name, args.get(0), nid -> clog.cfg(nid));

    case "c_cfg_entry":
      return makeUnaryOperation(name, args.get(0), nid -> clog.cfgEntry(nid));

    case "c_dump":
      return makeUnaryOperation(name, args.get(0), nid -> internalizeString(clog.dump(nid)));

    case "c_kind":
      return makeUnaryOperation(name, args.get(0), nid -> internalizeString(clog.kind(nid)));

    case "c_is_integer_literal":
      return makeUnaryOperation(name, args.get(0), nid -> clog.isIntegerLiteral(nid) ? 1 : 0);

    case "c_integer_literal_value":
      return makeUnaryOperation(name, args.get(0), nid -> clog.integerLiteralValue(nid));

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

  public VectorVectorLong lookupAt(long matcherId, long nodeId) {
    VectorVectorLong matches = clog.matchAtNode(matcherId, nodeId);
    return matches;
  }

  public VectorVectorLong lookupFrom(long matcherId, long nodeId) {
    VectorVectorLong matches = clog.matchFromNode(matcherId, nodeId);
    return matches;
  }

  public static class ExternalLiteralPayload {
    public static enum Kind {
      PATTERN,
      PARENT,
      CFG_SUCC,
      CFG_EXIT,
      CFG_PRED
    }

    ObjLangASTNode pattern;
    Optional<String> rootVariable;
    Optional<String> subtreeVariable;
    Kind kind;

    public static ExternalLiteralPayload pattern(ObjLangASTNode pattern, Optional<String> rootVariable, Optional<String> subtreeVariable) {
      var ret =  new ExternalLiteralPayload();
      ret.kind = Kind.PATTERN;
      ret.pattern = pattern;
      ret.rootVariable = rootVariable;
      ret.subtreeVariable = subtreeVariable;
      return ret;
    }

    public static ExternalLiteralPayload parent() {
      var ret = new ExternalLiteralPayload();
      ret.kind = Kind.PARENT;
      return ret;
    }

    public static ExternalLiteralPayload cfgSucc() {
      var ret = new ExternalLiteralPayload();
      ret.kind = Kind.CFG_SUCC;
      return ret;
    }

    public static ExternalLiteralPayload cfgExit() {
      var ret = new ExternalLiteralPayload();
      ret.kind = Kind.CFG_EXIT;
      return ret;

    }

  }
}
