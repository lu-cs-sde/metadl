package clang;

import se.lth.sep.Category;
import se.lth.sep.Grammar;
import se.lth.sep.SPPFTrivialProductionRemover;

// Clog-specific grammar transformation
public class SPPFClogTrivialProductionRemover extends SPPFTrivialProductionRemover {
  public SPPFClogTrivialProductionRemover(Grammar g) {
    super(g);
  }

  @Override public boolean isBubleUpChild(Category p, Category c) {
    if (p.getName().equals("declarator"))
      return false;
    if (p.getName().equals("initializer"))
      return false;
    if (p.getName().equals("typedef_name"))
      return false;
    if (c.getName().equals("METAVARID"))
      return true;
    if (c.getName().equals("GAP"))
      return true;
    return false;
  }
}
