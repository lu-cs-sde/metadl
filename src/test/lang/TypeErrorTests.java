package lang;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.io.File;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import lang.ast.Program;
import lang.ast.TypeError;
import lang.io.FileUtil;
import java.util.TreeSet;

public class TypeErrorTests {
	static void compileAndCheckOutput(String input, String... output) {
		try {
			Program program = (Program) FileUtil.parse(input);
			TreeSet<TypeError> errs = program.typeErrors();
			assertEquals(output.length, errs.size(), "Unexpected number of errors");

			if (errs.size() == output.length) {
				int i = 0;
				for (TypeError e : errs) {
					assertEquals(output[i], e.reportPosition());
					++i;
				}
			}
		} catch(Exception e) {
			fail();
		}
	}


	static String ml(String... strings) {
		StringBuffer sb = new StringBuffer();
		for (String s : strings) {
			sb.append(s);
			sb.append("\n");
		}
		return sb.toString();
	}

	@Test public void test1() {
		String prog = "A(x) :- A(x), NEQ(0, 1).";
		compileAndCheckOutput(prog,
							  "1,1-1,4: Predicate A is not well-typed: undefined type at position 0.",
							  "1,9-1,12: Predicate A is not well-typed: undefined type at position 0.");
	}


	@Test public void test2() {
		String prog = "A(x, 0) :- A(x, \"String\").";
		compileAndCheckOutput(prog,
							  "1,1-1,7: Predicate A is not well-typed: undefined type at position 0, contradictory type at position 1.",
							  "1,12-1,25: Predicate A is not well-typed: undefined type at position 0, contradictory type at position 1.");
	}

	@Test public void test3() {
		String prog = "A(x, y) :- B(x, y), NEQ(cat(x + y, x), cat(x, y)).";
		compileAndCheckOutput(prog,
							  "1,1-1,7: Predicate A is not well-typed: contradictory type at position 0, contradictory type at position 1.",
							  "1,12-1,18: Predicate B is not well-typed: contradictory type at position 0, contradictory type at position 1.",
							  "1,25-1,37: Expected type String as argument 0 of cat((x)+(y), x), but got Bad.",
							  "1,29-1,33: Expected type Integer as argument 0 of (x)+(y), but got Bad.",
							  "1,40-1,48: Expected type String as argument 0 of cat(x, y), but got Bad.");
	}

	@Test public void test4() {
		String prog = ml("A(1).",
						 "B(\"String\").",
						 "C(x, y) :- A(x), B(y), NEQ(x, y).");
		compileAndCheckOutput(prog,
							  "1,1-1,4: Predicate A is not well-typed: contradictory type at position 0.",
							  "2,1-2,11: Predicate B is not well-typed: contradictory type at position 0.",
							  "3,1-3,7: Predicate C is not well-typed: contradictory type at position 0, contradictory type at position 1.",
							  "3,12-3,15: Predicate A is not well-typed: contradictory type at position 0.",
							  "3,18-3,21: Predicate B is not well-typed: contradictory type at position 0.");
	}
}
