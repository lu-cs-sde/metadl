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
import lang.ast.IntConstant;
import lang.ast.PredicateSymbol;
import lang.ast.Program;
import lang.ast.RealLiteral;
import lang.ast.StringConstant;
import lang.ast.Term;
import lang.ast.Variable;
import lang.ast.config.Description;
import lang.evaluation.TopDownBasicRecursive;
import lang.io.FileUtil;
import lang.relation.PseudoTuple;

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

		Util.compareOutput(sb.toString(), new File(dir, FileUtil.changeExtension(filename, "_relationTests.out")),
				new File(dir, FileUtil.changeExtension(filename, "_relationTests.expected")));
	}

	@DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Valid")
	@ValueSource(strings = { "relationsTest_1.in", "relationsTest_2.in", "relationsTest_3.in" })
	public void relationTestsValid(String filename) throws Exception {
		System.out.println("valid/" + filename);
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, filename));
		doTest(program, filename, TEST_DIRECTORY_VALID);
	}

	@DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Invalid")
	@ValueSource(strings = { "relationsTest_1.in", "relationsTest_2.in", "relationsTest_3.in" })
	public void relationTestsInvalid(String filename) throws Exception {
		System.out.println("invalid/" + filename);
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_INVALID, filename));
		doTest(program, filename, TEST_DIRECTORY_INVALID);
	}

	@Test
	public void tupleGroundFalse() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));
		assertTrue(!ps.isGround());
	}

	@Test
	public void tupleGroundTrue() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new StringConstant("A")).set(1, new IntConstant("1"))
				.instantiate(2, new StringConstant("C"));
		assertTrue(ps.isGround());
	}

	@Test
	public void testInstantiate_Valid() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));
		PseudoTuple ps2 = new PseudoTuple(null, 3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));
		assertTrue(ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Invalid() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("D"));
		PseudoTuple ps2 = new PseudoTuple(null, 3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));
		assertTrue(!ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Invalid2() {
		PseudoTuple ps = new PseudoTuple(null, 2).set(0, new Variable("x")).set(1, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(null, 2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("B"));
		assertTrue(!ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Invalid3() {
		PseudoTuple ps = new PseudoTuple(null, 1).set(0, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(null, 2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("B"));
		assertTrue(!ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Valid2() {
		PseudoTuple ps = new PseudoTuple(null, 2).set(0, new Variable("x")).set(1, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(null, 2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("A"));
		assertTrue(ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiate_Valid3() {
		PseudoTuple ps = new PseudoTuple(null, 0);
		PseudoTuple ps2 = new PseudoTuple(null, 0);
		assertTrue(ps.instantiableAs(ps2));
	}

	@Test
	public void testInstantiation() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));

		PseudoTuple ps2 = new PseudoTuple(null, 3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));

		TreeMap<Variable, Constant> expected = new TreeMap<Variable, Constant>(Term.termComparator);

		expected.put(new Variable("x1"), new StringConstant("A"));
		expected.put(new Variable("x2"), new StringConstant("B"));
		TreeMap<Variable, Constant> inst = ps.createInstantiationFrom(ps2);

		assertTrue(inst.equals(expected));
	}

	@Test
	public void testInstantiateWith() {
		PseudoTuple ps = new PseudoTuple(null, 3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));

		PseudoTuple ps2 = new PseudoTuple(null, 3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));

		TreeMap<Variable, Constant> inst = ps.createInstantiationFrom(ps2);
		PseudoTuple ps3 = new PseudoTuple(null, 3).set(0, new Variable("x2")).set(1, new Variable("x2")).set(2,
				new Variable("x1"));

		ps3.instantiateWith(inst);
		PseudoTuple expected = new PseudoTuple(null, 3).instantiate(0, new StringConstant("B"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("A"));
		assertTrue(expected.equals(ps3));
	}

	@Test
	public void testAllObjectGen() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_1.in"));
			TopDownBasicRecursive bte = new TopDownBasicRecursive();
			bte.loadEBDFacts(program, new Description());

			for (int k = 0; k != 8; ++k) {
				assertEquals(program.objects.size(), 5);
				Set<PseudoTuple> tuples = bte.allObjectTuples(program, k, null);
				TreeSet<PseudoTuple> unique = new TreeSet<>(tuples);
				assertEquals(unique.size(), (int) Math.pow(program.objects.size(), k));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testFreeVarsTuple() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
			HashSet<PredicateSymbol> preds = program.getFormalPredicate(0).predicates();
			RealLiteral rl = (RealLiteral) preds.iterator().next().literal();

			PseudoTuple ps = new PseudoTuple(rl);
			TreeSet<Variable> expected = new TreeSet<>(Term.termComparator);
			expected.add(new Variable("x"));
			expected.add(new Variable("y"));
			assertTrue(expected.equals(ps.freeVariables()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testFreeVarsTupleEmpty() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
			HashSet<PredicateSymbol> preds = program.getFormalPredicate(1).predicates();
			RealLiteral rl = (RealLiteral) preds.iterator().next().literal();

			PseudoTuple ps = new PseudoTuple(rl);
			TreeSet<Variable> expected = new TreeSet<>(Term.termComparator);
			assertTrue(expected.equals(ps.freeVariables()));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testAllInstantiations() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
			TopDownBasicRecursive bte = new TopDownBasicRecursive();
			bte.loadEBDFacts(program, new Description());

			HashSet<PredicateSymbol> preds = program.getFormalPredicate(0).predicates();
			RealLiteral rl = (RealLiteral) preds.iterator().next().literal();
			PseudoTuple ps = new PseudoTuple(rl);

			assertEquals(ps.freeVariables().size(), 2);
			HashSet<TreeMap<Variable, Constant>> instantMaps = bte.allInstantiations(program, ps.freeVariables());
			assertEquals(instantMaps.size(), program.objects.size() * program.objects.size());
			TreeSet<PseudoTuple> instants = new TreeSet<>();

			instantMaps.forEach(instMap -> {
				PseudoTuple tup = new PseudoTuple(ps);
				tup.instantiateWith(instMap);
				instants.add(tup);
			});
			assertEquals(instants.size(), program.objects.size() * program.objects.size());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
