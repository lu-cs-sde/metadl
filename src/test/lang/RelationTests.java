package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import lang.ast.Constant;
import lang.ast.List;
import lang.ast.PredicateSymbol;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.StringConstant;
import lang.ast.Term;
import lang.ast.Variable;
import lang.relation.PseudoTuple;
import lang.relation.ListUtil;

public class RelationTests {
	/** Directory where the test input files are stored. */
	private static final File TEST_DIRECTORY_VALID = new File("tests/relations/valid");
	private static final File TEST_DIRECTORY_INVALID = new File("tests/relations/invalid");

	public void doTest(Program p, String filename, File dir) {
		StringBuilder sb = new StringBuilder();
		HashSet<String> serrs = p.semanticErrors();
		for (String serr : serrs) {
			sb.append(serr).append("\n");
		}

		if (serrs.size() == 0) {
			p.collectInitialMetaInfo(sb);
			sb.append("\n");
		}

		Util.compareOutput(sb.toString(), new File(dir, Util.changeExtension(filename, "_relationTests.out")),
				new File(dir, Util.changeExtension(filename, "_relationTests.expected")));
	}

	@DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Valid")
	@ValueSource(strings = { "relationsTest_1.in", "relationsTest_2.in", "relationsTest_3.in" })
	public void relationTestsValid(String filename) throws Exception {
		System.out.println("valid/" + filename);
		Program program = (Program) Util.parse(new File(TEST_DIRECTORY_VALID, filename));
		doTest(program, filename, TEST_DIRECTORY_VALID);
	}

	@DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Invalid")
	@ValueSource(strings = { "relationsTest_1.in", "relationsTest_2.in", "relationsTest_3.in" })
	public void relationTestsInvalid(String filename) throws Exception {
		System.out.println("invalid/" + filename);
		Program program = (Program) Util.parse(new File(TEST_DIRECTORY_INVALID, filename));
		doTest(program, filename, TEST_DIRECTORY_INVALID);
	}

	@Test
	public void tupleGround() {
		RealLiteral rl = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		assertTrue(!ps.isGround());
	}

	@Test
	public void testInstantiate_Valid() {
		RealLiteral rl = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		RealLiteral rl2 = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new StringConstant("A"), new StringConstant("B"), new StringConstant("C")));
		PseudoTuple ps2 = new PseudoTuple(rl2);
		assertTrue(ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Invalid() {
		RealLiteral rl = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		RealLiteral rl3 = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new StringConstant("A"), new StringConstant("B"), new StringConstant("D")));
		PseudoTuple ps3 = new PseudoTuple(rl3);
		assertTrue(!ps.instantiableAs(ps3));
	}

	@Test
	public void testInstantiate_Self() {
		RealLiteral rl = new RealLiteral(new PredicateSymbol("A"),
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		assertTrue(ps.instantiableAs(ps));
	}

	private static PredicateSymbol pred_A = new PredicateSymbol("A");

	@Test
	public void testInstantiation() {
		RealLiteral rl = new RealLiteral(pred_A,
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		RealLiteral rl2 = new RealLiteral(pred_A,
				new List<>(new StringConstant("A"), new StringConstant("B"), new StringConstant("C")));
		PseudoTuple ps2 = new PseudoTuple(rl2);
		TreeMap<Variable, Constant> expected = new TreeMap<Variable, Constant>(Term.termComparator);

		expected.put(new Variable("x1"), new StringConstant("A"));
		expected.put(new Variable("x2"), new StringConstant("B"));
		TreeMap<Variable, Constant> inst = ps.createInstantiationFrom(ps2);

		assertTrue(inst.equals(expected));
	}

	@Test
	public void testInstantiateWith() {
		RealLiteral rl = new RealLiteral(pred_A,
				new List<>(new Variable("x1"), new Variable("x2"), new StringConstant("C")));
		PseudoTuple ps = new PseudoTuple(rl);
		RealLiteral rl2 = new RealLiteral(pred_A,
				new List<>(new StringConstant("A"), new StringConstant("B"), new StringConstant("C")));
		PseudoTuple ps2 = new PseudoTuple(rl2);

		TreeMap<Variable, Constant> inst = ps.createInstantiationFrom(ps2);
		RealLiteral rl3 = new RealLiteral(pred_A,
				new List<>(new Variable("x2"), new Variable("x2"), new Variable("x1")));
		PseudoTuple ps3 = new PseudoTuple(rl3);
		ps3.instantiateWith(inst);

		RealLiteral el = new RealLiteral(pred_A,
				new List<>(new StringConstant("B"), new StringConstant("B"), new StringConstant("A")));
		PseudoTuple expected = new PseudoTuple(el);
		assertTrue(expected.equals(ps3));
	}

	@Test
	public void testAllInstations() {
		try {
			Program program = (Program) Util.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_1.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program);

			for (int k = 0; k != 10; ++k) {
				assertEquals(program.objects.size(), 5);
				Set<PseudoTuple> tuples = bte.allObjectTuples(program, k);
				TreeSet<PseudoTuple> unique = new TreeSet<>(tuples);
				assertEquals(unique.size(), (int) Math.pow(program.objects.size(), k));
				System.out.println(unique.size());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testToRadix() {
		int n = 4;
		int k = 2;
		for (int i = 0; i != (int) Math.pow(n, k); ++i) {
			java.util.List<Integer> l = ListUtil.toRadix(i, n, k);
			System.out.println("i: " + i + ", rad: " + l);
		}
	}
}
