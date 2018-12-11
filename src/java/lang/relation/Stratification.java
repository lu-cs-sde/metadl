package lang.relation;

import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Stack;
import java.util.TreeSet;

import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.io.SimpleLogger;

public class Stratification {
	public static HashSet<Stratum> strat = new HashSet<Stratum>();
	public static Deque<Stratum> order = new LinkedList<>();
	public static HashMap<FormalPredicate, Stratum> iso = new HashMap<>();
	private static int dfnum;

	/**
	 * Try all pairs and see if there is a negative dependency in the stratum
	 */
	private static void checkStratum(Stratum stratum) {
		stratum.forEach(fp1 -> {
			stratum.forEach(fp2 -> {
				if (FormalPredicate.hasNegativeDefUse(fp1, fp2)) {
					SimpleLogger.logger()
							.log("There exists a negative mutual recursion: " + fp1.predicateName() + " -> "
									+ fp2.predicateName() + " in stratum " + stratum,
									SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}
			});
		});
	}

	/**
	 * Tarjan's Strongly-Connect Components Alg.
	 */
	private static void strongConnect(FormalPredicate fp, Stack<FormalPredicate> stack,
			HashMap<FormalPredicate, NodeInfo> infoMap) {
		NodeInfo fpInfo = new NodeInfo(dfnum, dfnum);
		infoMap.put(fp, fpInfo);
		dfnum += 1;
		stack.push(fp);

		for (FormalPredicate w : fp.dependsOn()) {
			NodeInfo wInfo = infoMap.get(w);
			if (wInfo == null) { // Visited
				strongConnect(w, stack, infoMap);
				wInfo = infoMap.get(w);
				fpInfo.lowlink = Math.min(fpInfo.lowlink, wInfo.lowlink);
			} else if (wInfo.dfnum < fpInfo.dfnum && stack.contains(w)) { // Not visited
				fpInfo.lowlink = Math.min(fpInfo.lowlink, wInfo.dfnum);
			}
		}

		if (fpInfo.lowlink == fpInfo.dfnum) {
			Stratum scc = new Stratum();
			FormalPredicate w = null;
			do {
				w = stack.pop();
				scc.add(w);
				iso.put(w, scc);
			} while (w != fp);

			//SimpleLogger.logger().log("Found SCC: " + scc, SimpleLogger.LogLevel.Level.DEBUG);
			checkStratum(scc);
			strat.add(scc);
		}
	}

	private static void buildStratificationGraph() {
		for (Stratum fpStrat : strat) {
			for (FormalPredicate fp : fpStrat) {
				for (FormalPredicate w : fp.dependsOn()) {
					Stratum wStrat = iso.get(w);
					if (wStrat != fpStrat) {
						fpStrat.succ.add(wStrat);
					}
				}
			}
		}
	}

	private static void reversePostOrder(Stratum s, Deque<Stratum> order) {
		s.visited = true;
		for (Stratum w : s.succ) {
			if (!w.visited) {
				reversePostOrder(w, order);
			}
		}
		order.offerLast(s);
	}
	
	public static Deque<Stratum> reversePostOrder(HashSet<Stratum> output_strata) {
		Deque<Stratum> order = new LinkedList<>();
		for (Stratum s : output_strata) {
			if (!s.visited) {
				reversePostOrder(s, order);
			}
		}
		return order;
	}
	
	public static Deque<Stratum> order() {
		return reversePostOrder(strat);
	}
	

	private static void stratificationHelper(Program p) {
		dfnum = 0;
		HashMap<FormalPredicate, NodeInfo> infoMap = new HashMap<>();
		Stack<FormalPredicate> stack = new Stack<>();
		for (FormalPredicate fp : p.getFormalPredicates()) {
			if (infoMap.get(fp) == null && fp.literal().isInclusive()) {
				strongConnect(fp, stack, infoMap);
			}
		}
		buildStratificationGraph();
	}

	public static void stratification(Program p) {
		dfnum = 0;
		order = new LinkedList<>();
		strat = new HashSet<Stratum>();
		iso = new HashMap<>();
		stratificationHelper(p);
	}

	@SuppressWarnings("serial")
	public static class Stratum extends TreeSet<FormalPredicate> {
		public HashSet<Stratum> succ = new HashSet<>();
		public boolean visited = false;

		public Stratum() {
			super(FormalPredicate.formalPredicateComparator);
		}
	}

	private static class NodeInfo {
		public int dfnum;
		public int lowlink;

		public NodeInfo(int dfnum, int lowlink) {
			this.dfnum = dfnum;
			this.lowlink = lowlink;
		}
	}
}
