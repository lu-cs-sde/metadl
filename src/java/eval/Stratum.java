package eval;

import java.io.PrintStream;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import javax.management.relation.Relation;

public abstract class Stratum {
	List<Relation2> definedRelations;
	List<Control> statements;

	public Stratum(List<Relation2> definedRelations, List<Control> stmts) {
		this.definedRelations = definedRelations;
		this.statements = stmts;
	}

	abstract public void eval();
	abstract public void prettyPrint(PrintStream s);

	/**
	   Build a stratum that evaluates stmts until no more new facts can be derived
	 */
	public static Stratum fixpoint(List<Relation2> definedRelations, List<Control> stmts) {
		return new Stratum(definedRelations, stmts) {
			@Override public void eval() {
				boolean change;
				do {
					change = false;
					List<Integer> sizes = definedRelations.stream().map(r -> r.size()).collect(Collectors.toList());
					for (Control c : statements)
						c.eval();

					change = IntStream.range(0, definedRelations.size())
						.anyMatch(i -> sizes.get(i) != definedRelations.get(i).size());
				} while (change);
			}

			@Override public void prettyPrint(PrintStream s) {
				s.print("BEGIN FIXPOINT\n");

				for (Control c : statements) {
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
		return new Stratum(definedRelations, stmts) {
			@Override public void eval() {
				for (Control c : statements)
					c.eval();
			}

			@Override public void prettyPrint(PrintStream s) {
				s.print("BEGIN SINGLE\n");

				for (Control c : statements) {
					s.println(c.prettyPrint(0));
				}

				s.print("END SINGLE");
			}
		};
	}
}
