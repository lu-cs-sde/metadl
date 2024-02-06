package eval;

import java.util.ArrayList;
import java.util.List;

public class Sequence implements Control {
  private final List<Control> conts;

  public Sequence(List<Control> conts) {
    this.conts = new ArrayList<Control>(conts);
  }

  public void addCont(Control c) {
    conts.add(c);
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
