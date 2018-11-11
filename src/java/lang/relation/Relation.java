package lang.relation;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import lang.ast.Term;

public class Relation {
	public final PseudoTupleComparator pseudoTupleComparator = new PseudoTupleComparator();
	private Set<PseudoTuple> relation = new TreeSet<PseudoTuple>(pseudoTupleComparator);
	private int size;
	
	public Relation(int size) {
		this.size = size;
	}
	
	public class PseudoTupleComparator implements Comparator<PseudoTuple> {
		@Override
		public int compare(PseudoTuple tup1, PseudoTuple tup2) {
			for(int i = 0; i != size; ++i) {
				int res = Term.termComparator.compare(tup1.coord(i), tup2.coord(i));
				if(res != 0) return res;
			}
			return 0;
		}
	}
}
