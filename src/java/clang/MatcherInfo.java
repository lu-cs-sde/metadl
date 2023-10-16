package clang;

import java.util.Map;
import java.util.SortedSet;

import eval.Control;

public class MatcherInfo {
  public long matcherId;
  public int[] resultMap;
  public String matcher;
  public int atNodeIdx;
  public int fromNodeIdx;

  public static enum Kind {
    GLOBAL,
    AT,
    FROM
  }

  Kind kind;

  public static MatcherInfo global(String matcher, long matcherId, SortedSet<String> usedMetavars, Map<String, Integer> varToIdMap) {
    var r = new MatcherInfo();
    r.matcherId = matcherId;
    r.resultMap = new int[usedMetavars.size()];
    r.matcher = matcher;
    int i = 0;
    for (String v : usedMetavars) {
      r.resultMap[i++] = varToIdMap.get(v);
    }
    r.kind = Kind.GLOBAL;
    return r;
  }

  public static MatcherInfo matchAtNode(String matcher, long matcherId, SortedSet<String> usedMetavars, Map<String, Integer> varToIdMap,
                                        String atNodeVar) {
    var r = new MatcherInfo();
    r.matcherId = matcherId;
    r.resultMap = new int[usedMetavars.size()];
    r.matcher = matcher;
    r.atNodeIdx = varToIdMap.get(atNodeVar);
    int i = 0;
    for (String v : usedMetavars) {
      r.resultMap[i++] = varToIdMap.get(v);
    }
    r.kind = Kind.AT;
    return r;
  }

  public static MatcherInfo matchFromNode(String matcher, long matcherId, SortedSet<String> usedMetavars, Map<String, Integer> varToIdMap,
                                          String fromNodeVar) {
    var r = new MatcherInfo();
    r.matcherId = matcherId;
    r.resultMap = new int[usedMetavars.size()];
    r.matcher = matcher;
    r.fromNodeIdx = varToIdMap.get(fromNodeVar);
    int i = 0;
    for (String v : usedMetavars) {
      r.resultMap[i++] = varToIdMap.get(v);
    }
    r.kind = Kind.FROM;
    return r;
  }


  public Control genControl(ClangEvaluationContext ctx, Control cont) {
    switch (this.kind) {
    case GLOBAL:
      return new ExternalMatcher(ctx, this, cont);
    case AT:
      return new ExternalLocalMatcher(ctx, this, cont);
    case FROM:
      return new ExternalSubtreeMatcher(ctx, this, cont);
    }
    return null;
  }
}
