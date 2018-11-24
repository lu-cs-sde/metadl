package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.io.IOException;
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
import lang.ast.Rule;
import lang.ast.StringConstant;
import lang.ast.Term;
import lang.ast.Variable;
import lang.ast.config.Description;
import lang.evaluation.BottomUpNaiveIterative;
import lang.evaluation.TopDownBasicRecursive;
import lang.io.FileUtil;
import lang.relation.Binding;
import lang.relation.Binding.BindOverlap;
import lang.relation.Binding.BindResult;
import lang.relation.Instantiation;
import lang.relation.PseudoTuple;
import lang.relation.Relation;

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
	@ParameterizedTest(name = "Relation Tests Invalid")
	@ValueSource(strings = { "relationsTest_1.in", "relationsTest_2.in", "relationsTest_3.in" })
	public void relationTestsInvalid(String filename) throws Exception {
		System.out.println("invalid/" + filename);
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_INVALID, filename));
		doTest(program, filename, TEST_DIRECTORY_INVALID);
	}

	@Test
	public void tupleGroundFalse() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));
		assertTrue(!ps.isGround());
	}

	@Test
	public void tupleGroundTrue() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new StringConstant("A")).set(1, new IntConstant("1"))
				.instantiate(2, new StringConstant("C"));
		assertTrue(ps.isGround());
	}

	@Test
	public void testInstantiate_Valid() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));
		PseudoTuple ps2 = new PseudoTuple(3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));
		assertTrue(Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiate_Invalid() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("D"));
		PseudoTuple ps2 = new PseudoTuple(3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));
		assertTrue(!Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiate_Invalid2() {
		PseudoTuple ps = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("B"));
		assertTrue(!Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiate_Invalid3() {
		PseudoTuple ps = new PseudoTuple(1).set(0, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("B"));
		assertTrue(!Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiate_Valid2() {
		PseudoTuple ps = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("x"));
		PseudoTuple ps2 = new PseudoTuple(2).instantiate(0, new StringConstant("A")).instantiate(1,
				new StringConstant("A"));
		assertTrue(Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiate_Valid3() {
		PseudoTuple ps = new PseudoTuple(0);
		PseudoTuple ps2 = new PseudoTuple(0);
		assertTrue(Instantiation.instantiableAs(ps, ps2));
	}

	@Test
	public void testInstantiation() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));

		PseudoTuple ps2 = new PseudoTuple(3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));

		TreeMap<Variable, Constant> expected = new TreeMap<Variable, Constant>(Term.termComparator);

		expected.put(new Variable("x1"), new StringConstant("A"));
		expected.put(new Variable("x2"), new StringConstant("B"));
		TreeMap<Variable, Constant> inst = new Instantiation(ps, ps2); // .createInstantiationFrom(ps2);

		assertTrue(inst.equals(expected));
	}

	@Test
	public void testInstantiateWith() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));

		PseudoTuple ps2 = new PseudoTuple(3).instantiate(0, new StringConstant("A"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("C"));

		Instantiation inst = new Instantiation(ps, ps2); // ps.createInstantiationFrom(ps2);
		PseudoTuple ps3 = new PseudoTuple(3).set(0, new Variable("x2")).set(1, new Variable("x2")).set(2,
				new Variable("x1"));

		inst.instantiate(ps3);
		PseudoTuple expected = new PseudoTuple(3).instantiate(0, new StringConstant("B"))
				.instantiate(1, new StringConstant("B")).instantiate(2, new StringConstant("A"));
		assertTrue(expected.equals(ps3));
	}

	@Test
	public void testAllObjectGen() throws IOException, beaver.Parser.Exception {
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_1.in"));
		TopDownBasicRecursive bte = new TopDownBasicRecursive();
		bte.loadEBDFacts(program, new Description());

		for (int k = 0; k != 8; ++k) {
			assertEquals(program.objects.size(), 5);
			Set<PseudoTuple> tuples = bte.allObjectTuples(program, k);
			TreeSet<PseudoTuple> unique = new TreeSet<>(tuples);
			assertEquals(unique.size(), (int) Math.pow(program.objects.size(), k));
		}
	}

	@Test
	public void testFreeVarsTuple() throws IOException, beaver.Parser.Exception {
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
		HashSet<PredicateSymbol> preds = program.getFormalPredicate(0).predicates();
		RealLiteral rl = (RealLiteral) preds.iterator().next().literal();

		PseudoTuple ps = new PseudoTuple(rl);
		TreeSet<Variable> expected = new TreeSet<>(Term.termComparator);
		expected.add(new Variable("x"));
		expected.add(new Variable("y"));
		assertTrue(expected.equals(ps.freeVariables()));
	}

	@Test
	public void testFreeVarsTupleEmpty() throws IOException, beaver.Parser.Exception {
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
		HashSet<PredicateSymbol> preds = program.getFormalPredicate(1).predicates();
		RealLiteral rl = (RealLiteral) preds.iterator().next().literal();

		PseudoTuple ps = new PseudoTuple(rl);
		TreeSet<Variable> expected = new TreeSet<>(Term.termComparator);
		assertTrue(expected.equals(ps.freeVariables()));
	}

	@Test
	public void testAllInstantiations() throws IOException, beaver.Parser.Exception {
		Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_4.in"));
		TopDownBasicRecursive bte = new TopDownBasicRecursive();
		bte.loadEBDFacts(program, new Description());

		HashSet<PredicateSymbol> preds = program.getFormalPredicate(0).predicates();
		RealLiteral rl = (RealLiteral) preds.iterator().next().literal();
		PseudoTuple ps = new PseudoTuple(rl);

		assertEquals(ps.freeVariables().size(), 2);
		HashSet<Instantiation> instantMaps = bte.allInstantiations(program, ps.freeVariables());
		assertEquals(instantMaps.size(), program.objects.size() * program.objects.size());
		TreeSet<PseudoTuple> instants = new TreeSet<>();

		instantMaps.forEach(instMap -> {
			PseudoTuple tup = new PseudoTuple(ps);
			instMap.instantiate(tup);
			instants.add(tup);
		});
		assertEquals(instants.size(), program.objects.size() * program.objects.size());
	}

	@Test
	public void testBindingCreate() {
		PseudoTuple ps = new PseudoTuple(4).set(0, new Variable("x1")).set(1, new Variable("x2"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x1"));
		Binding b = Binding.createBinding(ps);
		assertTrue(b.toString().equals("[(x1 -> [0, 3]), (x2 -> [1]), (C -> [2])]"));
	}

	@Test
	public void testSelect1() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x2")).instantiate(2,
				new StringConstant("C"));
		Binding b = Binding.createBinding(ps);

		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("YES1"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C1"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("YES2"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		Relation selected = r.select(b);
		assertTrue(selected.tuples().toString().equals("[(A,YES1,C), (A,YES2,C), (B,YES3,C)]"));
	}

	@Test
	public void testSelect2() {
		PseudoTuple ps = new PseudoTuple(3).set(0, new Variable("x1")).set(1, new Variable("x1")).instantiate(2,
				new StringConstant("C"));
		Binding b = Binding.createBinding(ps);

		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("E")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		Relation selected = r.select(b);
		assertTrue(selected.tuples().toString().equals("[(A,A,C), (B,B,C)]"));
	}
	
	@Test
	public void testSelect3() {
		PseudoTuple ps = new PseudoTuple(2).set(0, new Variable("x1")).set(1, new Variable("x1"));
		Binding b = Binding.createBinding(ps);

		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("E")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		Relation selected = r.select(b);
		System.out.println(selected.tuples());
		assertTrue(selected.tuples().toString().equals("[(A,A,C), (B,B,C), (E,E,E)]"));
	}
	@Test
	public void testSelect4() {
		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("E")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		Relation selected = r.select(new Binding());
		assertTrue(selected.tuples().toString().equals("[(A,A,C), (A,C3,B), (B,B,C), (B,YES3,C), (E,E,E)]"));
	}
	
	@Test
	public void testSelectNamed1() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2,new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		
		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("E")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("x")).set(2,new Variable("y")).set(3, new Variable("z")).set(4,new StringConstant("C"));
		Binding b2 = Binding.createBinding(ps2);
		Relation s = r.select(b1);
		Relation expanded = s.selectNamed(b2);
		assertTrue(expanded.tuples().toString().equals("[(A,A,A,C,C), (A,A,C3,B,C), (B,B,B,C,C), (B,B,YES3,C,C), (E,E,E,E,C)]"));
	}
	
	@Test
	public void testSelectNamed2() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2,new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		
		Relation r = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("E")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("C3"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("YES3"), new StringConstant("C")));
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("UNC")).set(1, new StringConstant("D")).set(2,new Variable("y")).set(3, new Variable("z")).set(4,new StringConstant("C"));
		Binding b2 = Binding.createBinding(ps2);
		Relation s = r.select(b1);
		Relation expanded = s.selectNamed(b2);
		assertTrue(expanded.tuples().toString().equals("[(UNC,D,A,C,C), (UNC,D,B,C,C), (UNC,D,C3,B,C), (UNC,D,E,E,C), (UNC,D,YES3,C,C)]"));
	}

	@Test
	public void testBindingIntersect1() {
		PseudoTuple ps1 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("y"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x")).set(4, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("y")).set(1, new Variable("x"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("y")).set(4, new Variable("w"));
		Binding b2 = Binding.createBinding(ps2);
		TreeSet<BindOverlap> un = Binding.intersect(b1, b2);
		assertTrue(un.toString()
				.equals("[(x -> [0, 3]) ++ (x -> [1]), (y -> [1]) ++ (y -> [0, 3]), (C -> [2]) ++ (C -> [2])]"));
	}
	
	@Test
	public void testBindingDifference1() {
		PseudoTuple ps1 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("y"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x")).set(4, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("t1")).set(1, new Variable("x"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("t2")).set(4, new Variable("w"));
		Binding b2 = Binding.createBinding(ps2);
		TreeSet<BindOverlap> un = Binding.intersect(b1, b2);
		Binding bd = Binding.difference(un, b2);
		assertTrue(bd.toString()
				.equals("[(t1 -> [0]), (t2 -> [3]), (w -> [4])]"));
	}
	
	@Test
	public void testBindingDifference2() {
		PseudoTuple ps1 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("y"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x")).set(4, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("x"))
				.instantiate(2, new StringConstant("E")).set(3, new Variable("x")).set(4, new Variable("x"));
		Binding b2 = Binding.createBinding(ps2);
		TreeSet<BindOverlap> un = Binding.intersect(b1, b2);
		
		Binding bd = Binding.difference(un, b2);
		assertTrue(bd.toString()
				.equals("[(E -> [2])]"));
	}

	@Test
	public void testBindingMerge() {
		PseudoTuple ps1 = new PseudoTuple(5).set(0, new Variable("x")).set(1, new Variable("y"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x")).set(4, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);
		PseudoTuple ps2 = new PseudoTuple(5).set(0, new Variable("y")).set(1, new Variable("x"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("y")).set(4, new Variable("w"));
		Binding b2 = Binding.createBinding(ps2);
		TreeSet<BindOverlap> un = Binding.intersect(b1, b2);
		assertTrue(un.toString()
				.equals("[(x -> [0, 3]) ++ (x -> [1]), (y -> [1]) ++ (y -> [0, 3]), (C -> [2]) ++ (C -> [2])]"));

		BindResult br = Binding.merge(un, b1, b2);
		assertTrue(br.b_merged.toString().equals("[(w -> [3]), (x -> [0]), (y -> [1]), (z -> [2])]"));
		assertTrue(br.f.toString().equals(
				"{(w -> [3])=(w -> [4]) Right, (x -> [0])=(x -> [0, 3]) ++ (x -> [1]) Both, (y -> [1])=(y -> [1]) ++ (y -> [0, 3]) Both, (z -> [2])=(z -> [4]) Left}"));
	}
	
	
	@Test
	public void testJoin1() {
		PseudoTuple ps1 = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("y"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("B")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("E")));
		
		PseudoTuple ps2 = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("F"), new StringConstant("Z1")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("Z2")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("Z3")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("Z4")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.tuples().toString().equals("[(A,B,Z2), (A,B,Z4), (B,C,Z3)]"));
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [1]), (z -> [2])]"));
	}
	
	@Test
	public void testJoin2() {
		PseudoTuple ps1 = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("y"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("E")));
		
		PseudoTuple ps2 = new PseudoTuple(2).set(0, new Variable("w")).set(1, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("B"), new StringConstant("Z3")),
				PseudoTuple.of(new StringConstant("A"), new StringConstant("Z4")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(w -> [2]), (x -> [0]), (y -> [1]), (z -> [3])]"));
		assertTrue(j.tuples().toString().equals("[(B,C,A,Z4), (B,C,B,Z3), (D,E,A,Z4), (D,E,B,Z3)]"));
	}
	
	@Test
	public void testJoin3() {
		PseudoTuple ps1 = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("y"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("E")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r1.select(b1);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [1])]"));
		assertTrue(j.tuples().toString().equals("[(B,C), (D,E)]"));
	}
	
	@Test
	public void testJoin4() {
		PseudoTuple ps1 = new PseudoTuple(2).set(0, new Variable("x")).set(1, new Variable("y"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("B"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("E")));
		
		PseudoTuple ps2 = new PseudoTuple(2).set(0, new Variable("w")).set(1, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = new Relation(2);
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(w -> [2]), (x -> [0]), (y -> [1]), (z -> [3])]"));
		assertTrue(j.tuples().toString().equals("[]"));
	}
	
	@Test
	public void testJoin5() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("x")).set(2, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C1")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C3")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C5")));
		
		PseudoTuple ps2 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("F3")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("Y1"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("Y2"), new StringConstant("C3")),
				PseudoTuple.of(new StringConstant("F1"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("F2"), new StringConstant("E"), new StringConstant("C5")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [2]), (z -> [1])]"));
		assertTrue(j.tuples().toString().equals("[(B,C2,Y1), (C,C3,Y2)]"));
	}
	
	@Test
	public void testJoin6() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2, new StringConstant("C"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C")));
		
		PseudoTuple ps2 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new StringConstant("D")).set(2, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("F3")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("D"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("D"), new StringConstant("C3")),
				PseudoTuple.of(new StringConstant("F1"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("F2"), new StringConstant("D"), new StringConstant("C5")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [1]), (z -> [2])]"));
		assertTrue(j.tuples().toString().equals("[(C,C,C3)]"));
		
	}
	
	
	@Test
	public void testJoin7() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2, new StringConstant("C"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C")));
		
		PseudoTuple ps2 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new StringConstant("C")).set(2, new Variable("z"));
		Binding b2 = Binding.createBinding(ps2);
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("F3")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("C"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C3")),
				PseudoTuple.of(new StringConstant("F1"), new StringConstant("C"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("F2"), new StringConstant("C"), new StringConstant("C5")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r2.select(b2);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [1]), (z -> [2])]"));
		assertTrue(j.tuples().toString().equals("[(C,C,C3)]"));
		
	}
	
	@Test
	public void testJoin8() {
		PseudoTuple ps1 = new PseudoTuple(3).set(0, new Variable("x")).set(1, new Variable("y")).set(2, new Variable("z"));
		Binding b1 = Binding.createBinding(ps1);

		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C")));
		
		Relation s1 = r1.select(b1);
		Relation s2 = r1.select(b1);
		Relation j = Relation.join(s1, s2);
		assertTrue(j.binding.toString().equals("[(x -> [0]), (y -> [1]), (z -> [2])]"));
		assertTrue(j.tuples().toString().equals("[(A,A,C), (B,B,C2), (C,C,C), (D,D,C4), (E,E,C)]"));
		
	}

	@Test
	public void testImmediateConsequence() throws IOException, beaver.Parser.Exception {
		Description descr = FileUtil.parseDescription("internal::bottomupnaive ./tests/evaluation/evalTest_1.in");
		Program program = (Program) FileUtil.parse(new File(descr.getInput().getPath()));
		BottomUpNaiveIterative eval = (BottomUpNaiveIterative)descr.evaluationMethod();
		eval.loadEBDFacts(program, descr);
		program.getStmtList().forEach(s -> {
			if(s.isRule()) {
				Rule r = (Rule) s;
				eval.immediateConsequence(r);
				assertTrue(r.getHeads(0).getPredicate().formalpredicate().relation.tuples().toString().equals("[(A,B), (A,C), (B,C)]"));
				System.out.println(r.getHeads(0).getPredicate().formalpredicate().relation.tuples());
			}
		});
		//eval.immediateConsequence(r);
	}
}
