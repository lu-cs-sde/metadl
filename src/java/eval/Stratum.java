package eval;

import java.io.PrintStream;
import java.util.List;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import lang.io.SimpleLogger;
import static prof.Profile.profile;


public interface Stratum {
	public void eval();
	public void prettyPrint(PrintStream s);

	/**
	   Build a stratum that evaluates stmts until no more new facts can be derived
	   definedRelations = a list of relations defined in this stratum, as a (Rel, nextRel, deltaRel) pair.
	   stmts = the list of control statements to execute paired with the number of variables in the statement
	 */
	public static Stratum fixpoint(List<Triple<Relation2, Relation2, Relation2>> definedRelations,
								   List<Pair<Control, Integer>> stmts,
								   String desc) {
		return new Stratum() {
			@Override public void eval() {

				SimpleLogger.logger().time("=== Run stratum:\n");
				SimpleLogger.logger().time(desc);

				StopWatch timer = StopWatch.createStarted();
				profile().startTimer("strata", String.format("%x", desc.hashCode()));
				boolean change;

				// copy from rel to delta; rel may contain EDB tuples
				for (Triple<Relation2, Relation2, Relation2> p : definedRelations) {
						Relation2 rel  = p.getLeft();
						Relation2 delta = p.getRight();
						delta.insert(rel.tuples());
				}

				do {
					change = false;

					// evaluate all statements
					for (Pair<Control, Integer> c : stmts) {
						int nVars = c.getRight();
						Control stmt = c.getLeft();
						stmt.parallelEval(nVars);
					}

					// insert the deltas into the main relations; if there's any
					// change, then loop again
					for (Triple<Relation2, Relation2, Relation2> p : definedRelations) {
						Relation2 rel = p.getLeft();
						Relation2 next = p.getMiddle();
						Relation2 delta = p.getRight();

						delta.clear();
						for (Tuple t : next.tuples()) {
							boolean newTuple = rel.insert(t);
							if (newTuple) {
								change = true;
								delta.insert(t);
							}
						}
						next.clear();
					}
				} while (change);
				profile().stopTimer("strata", String.format("%x", desc.hashCode()));
				timer.stop();

				SimpleLogger.logger().time("=== " + timer.getTime() + " ms");
				SimpleLogger.logger().time("===========================================");
			}

			@Override public void prettyPrint(PrintStream s) {
				s.print(String.format("BEGIN FIXPOINT [%x]\n", desc.hashCode()));
				s.println(desc);

				for (Pair<Control, Integer> c : stmts) {
					s.println(c.getLeft().prettyPrint(0));
				}

				s.print("END FIXPOINT");
			}
		};
	}

	/**
	   Build a stratum that evaluates stmts once
	 */
	public static Stratum single(List<Control> stmts, int nEvalVariables, String desc) {
		return new Stratum() {
			@Override public void eval() {
				SimpleLogger.logger().time("=== Run stratum:\n");
				SimpleLogger.logger().time(desc);

				StopWatch timer = StopWatch.createStarted();
				profile().startTimer("strata", String.format("%x", desc.hashCode()));
				for (Control c : stmts) {
					Tuple t = new Tuple(nEvalVariables);
					c.eval(t);
				}
				profile().stopTimer("strata", String.format("%x", desc.hashCode()));
				timer.stop();

				SimpleLogger.logger().time("=== " + timer.getTime() + " ms");
				SimpleLogger.logger().time("===========================================");

			}

			@Override public void prettyPrint(PrintStream s) {
				s.print(String.format("BEGIN SINGLE [%x]\n", desc.hashCode()));
				s.println(desc);

				for (Control c : stmts) {
					s.println(c.prettyPrint(0));
				}

				s.print("END SINGLE");
			}
		};
	}
}
