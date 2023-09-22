package clang;

import clang.ClangEvaluationContext.MatcherInfo;
import clang.swig.VectorLong;
import clang.swig.VectorVectorLong;
import eval.Control;
import eval.Tuple;
import static prof.Profile.profile;

public class ExternalMatcher implements Control {
  private ClangEvaluationContext ctx;
  private Control cont;
  private MatcherInfo matcher;

  /**
     test - pairs of (column, tuple position) to test for equality
     assign - pairs of (column, tuple position) to assign
     consts - pairs of (column, value) of constants to assign
     arity - the arity of the external relation
  */
  public ExternalMatcher(ClangEvaluationContext ctx,
                        MatcherInfo matcher,
                        Control cont) {
    this.ctx = ctx;
    this.cont = cont;
    this.matcher = matcher;
    this.ID = (cont.prettyPrint(0) + matcher.matcher).hashCode();
  }

  @Override public void eval(Tuple t) {
    VectorVectorLong rows = ctx.lookup(matcher.matcherId);
    for (VectorLong row : rows) {
      for (int i = 0; i < matcher.resultMap.length; ++i) {
        if (matcher.resultMap[i] >= 0) {
          t.set(matcher.resultMap[i], row.get(i));
        }
      }

      cont.eval(t);
    }

    profile().addCounter("loop_count", String.format("%x", ID), rows.size());
  }

  private final int ID;

  @Override public String prettyPrint(int indent) {
    String s = Control.Util.indent(indent) + String.format("[%x] EXTERNAL FOR t IN %s WHERE ", ID, matcher.matcher);

    return s + "\n" + cont.prettyPrint(indent + 1);
  }
}
