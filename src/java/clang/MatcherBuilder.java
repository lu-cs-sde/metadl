package clang;

import java.util.ArrayList;
import java.util.List;

interface AbstractMatcherBuilder {
    String generate();
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
    return s;
  }


  public static MatcherBuilder match(String name, AbstractMatcherBuilder ... inner) {
    return new MatcherBuilder(name, inner);
  }

  public static ConstMatcherBuilder cst(String name) {
    return new ConstMatcherBuilder(name);
  }

}
