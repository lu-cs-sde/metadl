package clang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

interface AbstractMatcherBuilder {
  String generate();

  default boolean hasBinding() {
    return false;
  }

  default String getBinding() {
    if (!hasBinding())
      throw new RuntimeException("No binding for this matcher.");
    return null;
  }

  default List<AbstractMatcherBuilder> getChildren() {
    return Collections.emptyList();
  }
}

class ConstMatcherBuilder implements AbstractMatcherBuilder {
  private String name;
  public ConstMatcherBuilder(String name) {
    this.name = name;
  }
  @Override public String generate() {
    return "\"" + name + "\"";
  }
}

class ConstIntMatcherBuilder implements AbstractMatcherBuilder {
  private int value;
  public ConstIntMatcherBuilder(int value) {
    this.value = value;
  }
  @Override public String generate() {
    return "" + value;
  }
}


public class MatcherBuilder implements AbstractMatcherBuilder {
  private String name;
  private String metavar;
  private List<AbstractMatcherBuilder> inner;

  public MatcherBuilder(String name, AbstractMatcherBuilder ... innerMatchers) {
    this.name = name;
    this.inner = new ArrayList<AbstractMatcherBuilder>(List.of(innerMatchers));
  }

  public MatcherBuilder add(AbstractMatcherBuilder innerMatcher) {
    inner.add(innerMatcher);
    return this;
  }

  public MatcherBuilder bind(String name) {
    if (this.metavar != null) {
      throw new IllegalStateException("The matcher already binds a variable.");
    }
    this.metavar = name;
    return this;
  }

  @Override
  public List<AbstractMatcherBuilder> getChildren() {
    return this.inner;
  }

  @Override public boolean hasBinding() {
    return this.metavar != null;
  }

  @Override public String getBinding() {
    if (!hasBinding())
      throw new RuntimeException("No binding for this matcher.");
    return this.metavar;
  }

  @Override public String generate() {
    String s = name + "(";
    boolean firstElem = true;
    for (AbstractMatcherBuilder i : inner) {
      if (!firstElem) {
        s += ", ";
      } else {
        firstElem = false;
      }
      s += i.generate();
    }
    s += ")";

    if (metavar != null) {
      s += ".bind(\"" + metavar  + "\")";
    }

    return s;
  }

  private static void collectBindings(AbstractMatcherBuilder b, SortedSet<String> bindings) {
    if (b.hasBinding()) {
      if (!bindings.add(b.getBinding())) {
        // TODO: Proper error reporting
        throw new RuntimeException(String.format("Variable '%s' is bound twice in a pattern.", b.getBinding()));
      }
    }
    for (AbstractMatcherBuilder ib : b.getChildren()) {
      collectBindings(ib, bindings);
    }
  }

  public SortedSet<String> bindings() {
    SortedSet<String> ret = new TreeSet<String>();
    collectBindings(this, ret);
    return ret;
  }

  public static MatcherBuilder match(String name, AbstractMatcherBuilder ... inner) {
    return new MatcherBuilder(name, inner);
  }

  public static ConstMatcherBuilder cst(String name) {
    return new ConstMatcherBuilder(name);
  }

  public static ConstIntMatcherBuilder integer(int v){
    return new ConstIntMatcherBuilder(v);
  }

  public static MatcherBuilder unless(MatcherBuilder b) {
    return new MatcherBuilder("unless", b);
  }

  public static MatcherBuilder anything() {
    return new MatcherBuilder("anything");
  }

}
