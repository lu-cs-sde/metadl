package eval;

import java.io.PrintStream;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;


public interface Stratum {
	public void eval();
	public void prettyPrint(PrintStream s);

	/**
	   Build a stratum that evaluates stmts until no more new facts can be derived
	   definedRelations = a list of relations defined in this stratum, as a (Rel, nextRel, deltaRel) pair.
	 */
	public static Stratum fixpoint(List<Triple<Relation2, Relation2, Relation2>> definedRelations, List<Control> stmts) {
		return new Stratum() {
			@Override public void eval() {
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
					for (Control c : stmts)
						c.eval();

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
			}

			@Override public void prettyPrint(PrintStream s) {
				s.print("BEGIN FIXPOINT\n");

				for (Control c : stmts) {
					s.println(c.prettyPrint(0));
				}

				s.print("END FIXPOINT");
			}
		};
	}

	/**
	   Build a stratum that evaluates stmts once
	 */
	public static Stratum single(List<Relation2> definedRelations, List<Control> stmts) {
		return new Stratum() {
			@Override public void eval() {
				for (Control c : stmts)
					c.eval();
			}

			@Override public void prettyPrint(PrintStream s) {
				s.print("BEGIN SINGLE\n");

				for (Control c : stmts) {
					s.println(c.prettyPrint(0));
				}

				s.print("END SINGLE");
			}
		};
	}
}
