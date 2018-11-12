package lang.relation;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ListUtil<T> {
	public static List<Integer> toRadix(int nbr, int base, int minSize) {
		List<Integer> rad = new ArrayList<Integer>();
		
		while(nbr > 0) {
			int res = nbr % base;
			rad.add(res);
			nbr = (nbr - res) / base;
		}

		while(rad.size() < minSize) {
			rad.add(0);
		}
		
		return rad;
	}
}
