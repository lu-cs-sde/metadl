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
	private static boolean isComputed = false;
	private static HashSet<Stratum> strat = new HashSet<Stratum>();
	private static Deque<Stratum> order = new LinkedList<>();
	private static HashMap<FormalPredicate, Stratum> iso = new  HashMap<>();
	private static int dfnum;
	
	/**
	 * Tarjan's Strongly-Connect Components Alg.
	 */
	private static void strongConnect(FormalPredicate fp, Stack<FormalPredicate> stack, HashMap<FormalPredicate, NodeInfo> infoMap) {
		NodeInfo fpInfo = new NodeInfo(dfnum, dfnum);
		infoMap.put(fp, fpInfo);
		dfnum += 1;
		stack.push(fp);
		
		for(FormalPredicate w : fp.dependsOn()) {
			NodeInfo wInfo = infoMap.get(w);
			if(wInfo == null) { // Visited
				strongConnect(w, stack, infoMap);
				wInfo = infoMap.get(w);
				fpInfo.lowlink = Math.min(fpInfo.lowlink, wInfo.lowlink);
			} else if(wInfo.dfnum < fpInfo.dfnum && stack.contains(w)) { // Not visited
				fpInfo.lowlink = Math.min(fpInfo.lowlink, wInfo.dfnum);
			}
		}
		
		if(fpInfo.lowlink == fpInfo.dfnum) {
			Stratum scc = new Stratum();
			FormalPredicate w = null;
			do {
				w = stack.pop();
				scc.add(w);
				iso.put(w, scc);
			} while(w != fp);
			
			SimpleLogger.logger().log("Found SCC: " + scc, SimpleLogger.LogLevel.Level.DEBUG);
			strat.add(scc);
		}
	}
	
	private static void buildStratificationGraph() {
		for(Stratum fpStrat : strat) {
			for(FormalPredicate fp : fpStrat) {
				for(FormalPredicate w : fp.dependsOn()) {
					Stratum wStrat = iso.get(w);
					if(wStrat != fpStrat) {
						fpStrat.succ.add(wStrat);
					}
				}
			}
		}
	}
	
	private static void reversePostOrder(Stratum s, Deque<Stratum> order) {
		s.visited = true;
		for(Stratum w : s.succ) {
			if(!w.visited) {
				reversePostOrder(w, order);
			}
		}
		order.offerLast(s);
	}
	
	private static Deque<Stratum> stratificationHelper(Program p) {
		dfnum = 0;
		HashMap<FormalPredicate, NodeInfo> infoMap = new HashMap<>();
		Stack<FormalPredicate> stack = new Stack<>();
		for(FormalPredicate fp : p.getFormalPredicates()) {
			if(infoMap.get(fp) == null) {
				strongConnect(fp, stack, infoMap);
			}
		}
		buildStratificationGraph();
		for(Stratum s : strat) {
			if(!s.visited) {
				reversePostOrder(s, order);
			}
		}
		Deque<Stratum> orderCpy = new LinkedList<>();
		order.forEach(s -> orderCpy.offerLast(s));
		isComputed = true;
		return orderCpy;
	}
	
	public static Deque<Stratum> stratification(Program p) {
		if(isComputed) {
			Deque<Stratum> orderCpy = new LinkedList<>();
			order.forEach(s -> orderCpy.offerLast(s));
			return orderCpy;
		}
		return stratificationHelper(p);
	}
	
	public static Deque<Stratum> stratificationForceCompute(Program p) {
		System.out.println("Begin Strat");
		dfnum = 0;
		order = new LinkedList<>();
		strat = new HashSet<Stratum>();
		iso = new  HashMap<>();
		Deque<Stratum> strat  = stratificationHelper(p);
		
		System.out.println("End Strat");
		return strat;
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
