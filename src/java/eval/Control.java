package eval;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.concurrent.Callable;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.tuple.Pair;

class Util {
  static String indent(int n) {
    String s = "";
    for (int i = 0; i < n; ++i)
      s += "\t";
    return s;
  }
}

public interface Control {
  default void parallelEval(int nVariables) { eval(new Tuple(nVariables)); }

  void eval(Tuple t);

  String prettyPrint(int indent);

  public static Control forAll(EvaluationContext ctx,
                               Relation2 rel, List<Pair<Integer, Integer>> test,
                               List<Pair<Integer, Long>> consts,
                               List<Pair<Integer, Integer>> assign,
                               Control cont) {
    return new ForAll(ctx, rel, test, consts, assign, cont);
  }

  public static Control ifNotExists(Relation2 rel, List<Pair<Integer, Integer>> test,
                                    List<Pair<Integer, Long>> consts,
                                    Control cont) {
    return new IfExists(rel, test, consts, cont, false);
  }

  public static Control ifExists(Relation2 rel, List<Pair<Integer, Integer>> test,
                                 List<Pair<Integer, Long>> consts,
                                 Control cont) {
    return new IfExists(rel, test, consts, cont, true);
  }

  public static Control insert(Relation2 rel, List<Pair<Integer, Integer>> assign,
                               List<Pair<Integer, Long>> consts, Control cont) {
    return new Insert(rel, assign, consts, cont);
  }

  public static Control sequence(List<Control> conts) {
    return new Sequence(conts);
  }

  public static Control nop() {
    return new Control() {
      @Override public void eval(Tuple t) {
        // do nothing
      }

      @Override public String prettyPrint(int indent) {
        return "";
      }
    };
  }

  public static Control eq(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a == b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s = %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }
    };
  }

  public static Control neq(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a != b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s != %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }
    };
  }

  public static Control gt(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a > b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s > %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }
    };
  }

  public static Control lt(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a < b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s < %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }

    };
  }

  public static Control gte(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a >= b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s >= %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }

    };
  }

  public static Control lte(Control cont, Operation l, Operation r) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        return a <= b;
      }
      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %s <= %s THEN\n", l.prettyPrint(), r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }

    };
  }

  public static Control match(EvaluationContext ctx, Control cont, Operation regex, Operation arg, boolean invert) {
    return new Test(cont, regex, arg) {
      @Override public boolean test(long a, long b) {
        String r = ctx.externalizeString(a);
        String s = ctx.externalizeString(b);
        return invert ^ Pattern.matches(r, s);
      }

      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %smatch(%s, %s) THEN\n", invert ? "!" : "", regex.prettyPrint(), arg.prettyPrint());
      }
    };
  }

  public static Control bind(Control cont, int dst, Operation r) {
    return new Control() {
      @Override public void eval(Tuple t) {
        t.set(dst, r.eval(t));
        cont.eval(t);
      }

      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("t[%d] := %s\n", dst, r.prettyPrint())
          + cont.prettyPrint(indent + 1);
      }
    };
  }

  public static Control ancestor(Control cont, Operation l, Operation r, boolean invert) {
    return new Test(cont, l, r) {
      @Override public boolean test(long a, long b) {
        boolean r = (clang.DatalogProjection.CNodeIdDesc.fileId(a) ==
                     clang.DatalogProjection.CNodeIdDesc.fileId(b)) &&
          (clang.DatalogProjection.CNodeIdDesc.preOrdNum(a) <=
           clang.DatalogProjection.CNodeIdDesc.preOrdNum(b)) &&
          (clang.DatalogProjection.CNodeIdDesc.postOrdNum(b) <=
           clang.DatalogProjection.CNodeIdDesc.postOrdNum(a));

        return invert ^ r;
      }

      @Override public String prettyPrint(int indent) {
        return Util.indent(indent) + String.format("IF %sancestor(%s, %s) THEN\n", invert ? "!" : "", l.prettyPrint(), r.prettyPrint());
      }
    };
  }
}

class ForAll implements Control {
  private Relation2 rel;
  private List<Pair<Integer, Integer>> test;
  private List<Pair<Integer, Integer>> assign;
  private List<Pair<Integer, Long>> consts;
  private EvaluationContext ctx;

  private Control cont;
  private Relation2.ReadOnlyView view;
  /**
     test - pairs of (columns, tuple position) to test for equality
     assign - pairs of (columns, tuple position) to assign
  */
  ForAll(EvaluationContext ctx, Relation2 rel,
         List<Pair<Integer, Integer>> test, List<Pair<Integer, Long>> consts,
         List<Pair<Integer, Integer>> assign, Control cont) {

    this.rel = rel;
    this.cont = cont;
    this.test = test;
    this.consts = consts;
    this.assign = assign;
    this.ctx = ctx;
    Index index = new Index(Stream.concat(test.stream(), consts.stream())
                            .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());
    this.view = rel.getReadOnlyView(index);
  }

  public void eval(Tuple t) {
    Tuple minKey = rel.infTuple();
    Tuple maxKey = rel.supTuple();

    for (Pair<Integer, Long> c : consts) {
      minKey.set(c.getLeft(), c.getRight());
      maxKey.set(c.getLeft(), c.getRight());
    }

    // populate the variable part of the prefix
    for (Pair<Integer, Integer> c : test) {
      minKey.set(c.getLeft(), t.get(c.getRight()));
      maxKey.set(c.getLeft(), t.get(c.getRight()));
    }

    SortedSet<Tuple> tuples = view.lookup(minKey, maxKey);
    for (Tuple r : tuples) {
      for (Pair<Integer, Integer> p : assign) {
        t.set(p.getRight(), r.get(p.getLeft()));
      }
      cont.eval(t);
    }
  }

  @Override public void parallelEval(int nVariables) {
    if (!test.isEmpty()) {
      throw new RuntimeException("Parallel eval can be called only when no variables are bound (e.g. for the first literal in a clause).");
    }

    Tuple minKey = rel.infTuple();
    Tuple maxKey = rel.supTuple();

    for (Pair<Integer, Long> c : consts) {
      minKey.set(c.getLeft(), c.getRight());
      maxKey.set(c.getLeft(), c.getRight());
    }

    SortedSet<Tuple> tuples = view.lookup(minKey, maxKey);

    List<Callable<Void>> callables = new ArrayList<>();
    for (Tuple r : tuples) {
      callables.add(new Callable<Void>() {
          @Override public Void call() {
            Tuple t = new Tuple(nVariables);
            for (Pair<Integer, Integer> p : assign) {
              t.set(p.getRight(), r.get(p.getLeft()));
            }
            cont.eval(t);
            return null;
          }
        });
    }


    try {
      ctx.getExecutorService().invokeAll(callables);
    } catch (InterruptedException e) {
      throw new RuntimeException(e);
    }
  }

  @Override public String prettyPrint(int indent) {
    String s = Util.indent(indent) + String.format("FOR t IN %s WHERE ", rel.getName());

    for (Pair<Integer, Integer> t : test) {
      s += String.format("%s[%d] = t[%d] ", rel.getName(), t.getLeft(), t.getRight());
    }

    for (Pair<Integer, Long> c : consts) {
      s += String.format("%s[%d] = %d ", rel.getName(), c.getLeft(), c.getRight());
    }

    for (Pair<Integer, Integer> a : assign) {
      s += String.format("t[%d] := %s[%d] ", a.getRight(), rel.getName(), a.getLeft());
    }

    return s + "\n" + cont.prettyPrint(indent + 1);
  }

  @Override public String toString() {
    return prettyPrint(0);
  }
}


class IfExists implements Control {
  private Relation2 rel;
  private List<Pair<Integer, Integer>> test;
  private List<Pair<Integer, Long>> consts;
  private Control cont;
  private Relation2.ReadOnlyView view;
  private final boolean positive;

  IfExists(Relation2 rel, List<Pair<Integer, Integer>> test,
           List<Pair<Integer, Long>> consts, Control cont, boolean positive) {
    this.rel = rel;
    this.cont = cont;
    this.consts = consts;
    this.test = test;
    this.positive = positive;
    Index index = new Index(Stream.concat(test.stream(), consts.stream())
                            .map(p -> p.getLeft()).collect(Collectors.toList()), rel.arity());
    this.view = rel.getReadOnlyView(index);
  }

  @Override public void eval(Tuple t) {
    Tuple minKey = rel.infTuple();
    Tuple maxKey = rel.supTuple();

    // populate the constant part
    for (Pair<Integer, Long> c : consts) {
      minKey.set(c.getLeft(), c.getRight());
      maxKey.set(c.getLeft(), c.getRight());
    }

    // populate the variable part of the prefix
    for (Pair<Integer, Integer> c : test) {
      minKey.set(c.getLeft(), t.get(c.getRight()));
      maxKey.set(c.getLeft(), t.get(c.getRight()));
    }

    if (positive ^ !view.hasEntryInRange(minKey, maxKey))
      cont.eval(t);
  }

  @Override public String prettyPrint(int indent) {
    String s = Util.indent(indent) + String.format("IF (");
    for (Pair<Integer, Integer> t : test) {
      s += String.format("%d:t[%d], ", t.getLeft(), t.getRight());
    }

    for (Pair<Integer, Long> c : consts) {
      s += String.format("%d:%d, ", c.getLeft(), c.getRight());
    }

    s += ") " + (positive ? "" : "NOT") + " IN " + rel.getName() + " THEN\n" + cont.prettyPrint(indent + 1);
    return s;
  }

  @Override public String toString() {
    return prettyPrint(0);
  }
}

class Insert implements Control {
  private List<Pair<Integer, Integer>> assign;
  private List<Pair<Integer, Long>> consts;
  private Control cont;
  private Relation2 rel;

  /**
     t - tuple to draw the data from
     rel - relation to insert into
     assign - list of (column, tuple position) to copy data from tuple entry to colum
     consts - list of (column, const) to insert const into column
     cont - continuation
  */
  Insert(Relation2 rel, List<Pair<Integer, Integer>> assign, List<Pair<Integer, Long>> consts, Control cont) {
    this.rel = rel;
    this.assign = assign;
    this.consts = consts;
    this.cont = cont;
  }

  @Override public void eval(Tuple t) {
    Tuple u = new Tuple(rel.arity());

    for (Pair<Integer, Integer> a : assign) {
      u.set(a.getLeft(), t.get(a.getRight()));
    }

    for (Pair<Integer, Long> c : consts) {
      u.set(c.getLeft(), c.getRight());
    }

    rel.insert(u);

    cont.eval(t);
  }

  @Override public String prettyPrint(int indent) {
    String s = Util.indent(indent) + String.format("INSERT (");

    for (Pair<Integer, Integer> a : assign) {
      s += String.format("%d:t[%d], ", a.getLeft(), a.getRight());
    }

    for (Pair<Integer, Long> c : consts) {
      s += String.format("%d:%d, ", c.getLeft(), c.getRight());
    }

    s += ") INTO " + rel.getName() + "\n" + cont.prettyPrint(indent + 1);

    return s;
  }

  @Override public String toString() {
    return prettyPrint(0);
  }
}

class Sequence implements Control {
  private List<Control> conts;

  Sequence(List<Control> conts) {
    this.conts = conts;
  }

  @Override public void eval(Tuple t) {
    for (Control c : conts) {
      // Tuple tt = t.clone();
      c.eval(t);
    }
  }

  @Override public String prettyPrint(int indent) {
    String s  = Util.indent(indent) + "SEQUENCE\n";
    for (Control c : conts) {
      s += c.prettyPrint(indent + 1);
    }
    return s;
  }

  @Override public String toString() {
    return prettyPrint(0);
  }
}

abstract class Test implements Control {
  private Control cont;
  private Operation left, right;

  public Test(Control cont, Operation left, Operation right) {
    this.cont = cont;
    this.left = left;
    this.right = right;
  }

  public void eval(Tuple t) {
    if (test(left.eval(t), right.eval(t)))
      cont.eval(t);
  }

  @Override public String toString() {
    return prettyPrint(0);
  }

  protected abstract boolean test(long a, long b);
}
