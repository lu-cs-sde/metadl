package lang.relation;
import java.util.TreeMap;
import lang.ast.Term;
import lang.ast.Variable;

@SuppressWarnings("serial")
public class Instantiation extends TreeMap<Variable, Term> {
	public Instantiation() {
		super(Term.termComparator);
	}
	
	/**
	 * Creates an Instantiation from ground pt.
	 */
	public Instantiation(PseudoTuple from, PseudoTuple pt) {
		super(Term.termComparator);
		for(int i = 0; i != from.arity(); ++i) {
			Term t1 = from.coord(i);
			Term t2 = pt.coord(i);
			if(t1.isVariable()) {
				put((Variable)t1, t2);
			}
		}
	}
	
	public void instantiate(PseudoTuple pt) {
		for(int i = 0; i != pt.arity(); ++i) {
			Term t = pt.coord(i);
			if(t.isVariable()) {
				Term c = get((Variable) t);
				if(c != null) {
					pt.set(i, c);
				}
			}
		}
	}
}
