package lang.relation;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.TreeMap;

import lang.ast.Constant;
import lang.ast.Term;
import lang.ast.Variable;

@SuppressWarnings("serial")
public class Instantiation extends TreeMap<Variable, Constant> {
	public Instantiation() {
		super(Term.termComparator);
	}
	
	/**
	 * Creates an Instantiation from ground pt.
	 */
	public Instantiation(PseudoTuple from, PseudoTuple pt) {
		super(Term.termComparator);
		assertTrue(pt.isGround());
		assertTrue(Instantiation.instantiableAs(from, pt));
		
		for(int i = 0; i != from.size; ++i) {
			Term t1 = from.coord(i);
			Term t2 = pt.coord(i);
			if(t1.isVariable()) {
				put((Variable)t1, (Constant)t2);
			}
		}
	}
	
	public void instantiate(PseudoTuple pt) {
		for(int i = 0; i != pt.size; ++i) {
			Term t = pt.coord(i);
			if(t.isVariable()) {
				Constant c = get((Variable) t);
				if(c != null) {
					pt.instantiate(i, c);
				}
			}
		}
	}
	
	public static boolean instantiableAs(PseudoTuple t, PseudoTuple pt) {
		if(t.size != pt.size) return false;
		Instantiation binding = new Instantiation();
		for(int i = 0; i != t.size; ++i) {
			Term t1 = t.coord(i);
			Term t2 = pt.coord(i);
			
			if(t1.isVariable()) {
				Constant c = binding.get((Variable)t1);
				if(c == null) 
					binding.put((Variable)t1, (Constant)t2);
				else if(!c.equals(t2)) 
					return false;
			} else if(!((Constant)t1).equals(t2)) return false;
		}
		return true;
	}
}
