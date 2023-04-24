package clang;

import clang.ClangEvaluationContext.MatcherInfo;
import clang.swig.VectorLong;
import clang.swig.VectorVectorLong;
import eval.Control;
import eval.Tuple;


public class ExternalForAll implements Control {
  private ClangEvaluationContext ctx;
  private String name;
  private Control cont;
  private MatcherInfo matcher;

  /**
     test - pairs of (column, tuple position) to test for equality
     assign - pairs of (column, tuple position) to assign
     consts - pairs of (column, value) of constants to assign
     arity - the arity of the external relation
  */
  public ExternalForAll(ClangEvaluationContext ctx,
                        MatcherInfo matcher,
                        Control cont) {
    this.ctx = ctx;
    this.cont = cont;
    this.matcher = matcher;
  }

  @Override public void eval(Tuple t) {
    VectorVectorLong rows = ctx.lookup(matcher.matcherId);
    for (VectorLong row : rows) {
      for (int i = 0; i < matcher.resultMap.length; ++i) {
        t.set(matcher.resultMap[i], row.get(i));
      }

      cont.eval(t);
    }
  }

  @Override public String prettyPrint(int indent) {
    String s = Control.Util.indent(indent) + String.format("EXTERNAL FOR t IN %s WHERE ", matcher.matcher);

    return s + "\n" + cont.prettyPrint(indent + 1);
  }
}
