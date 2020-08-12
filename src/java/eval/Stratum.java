package eval;

import java.io.PrintStream;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Stratum {
	List<Relation2> definedRelations;
	List<Control> statements;

	public Stratum(List<Relation2> definedRelations, List<Control> stmts) {
		this.definedRelations = definedRelations;
		this.statements = stmts;
	}

	public void eval() {
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

	public void prettyPrint(PrintStream s) {
		s.print("BEGIN STRATUM\n");

		for (Control c : statements) {
			s.println(c.prettyPrint(0));
		}

		s.print("END_STRATUM");
	}
}
