package lang;

import java.io.File;

import lang.ast.config.Description;

public class RelationTests {
	/** Directory where the test input files are stored. */
	private static final File TEST_DIRECTORY_VALID = new File("tests/relations/valid");
	private static final File TEST_DIRECTORY_INVALID = new File("tests/relations/invalid");
	
	private static final Description defaultDescr = new Description();
/*
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
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);

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
	public void testToRadix() {
		int n = 4;
		int k = 2;
		for (int i = 0; i != (int) Math.pow(n, k); ++i) {
			java.util.List<Integer> l = ListUtil.toRadix(i, n, k);
		}
	}

	@Test
	public void testFreeVarsTuple() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_6.in"));
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
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_6.in"));
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
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_6.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);

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

	@Test
	public void testDeriveFact1() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_7.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);
			FormalPredicate sp = program.getFormalPredicate(0);

			StringBuilder sb = new StringBuilder();
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(C,D)}"));

			PseudoTuple ps = new PseudoTuple(sp);
			ps.instantiate(0, new StringConstant("B")).instantiate(1, new StringConstant("A"));
			boolean hasProof = bte.deriveFact(ps, program);

			sb.setLength(0);
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(B,A)(C,D)}"));

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testDeriveFact2() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_7.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);
			FormalPredicate sp = program.getFormalPredicate(0);
			StringBuilder sb = new StringBuilder();
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(C,D)}"));

			PseudoTuple ps = new PseudoTuple(sp);
			ps.instantiate(0, new StringConstant("B")).instantiate(1, new StringConstant("A"));
			bte.deriveFact(ps, program);

			PseudoTuple ps2 = new PseudoTuple(sp);
			ps2.instantiate(0, new StringConstant("D")).instantiate(1, new StringConstant("C"));
			bte.deriveFact(ps2, program);
			sb.setLength(0);
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(B,A)(C,D)(D,C)}"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testDeriveFact3() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_7.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);
			FormalPredicate sp = program.getFormalPredicate(0);
			StringBuilder sb = new StringBuilder();
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(C,D)}"));

			PseudoTuple ps = new PseudoTuple(sp);
			ps.instantiate(0, new StringConstant("B")).instantiate(1, new StringConstant("A"));
			bte.deriveFact(ps, program);

			PseudoTuple ps2 = new PseudoTuple(sp);
			ps2.instantiate(0, new StringConstant("D")).instantiate(1, new StringConstant("C"));
			bte.deriveFact(ps2, program);
			sb.setLength(0);
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(B,A)(C,D)(D,C)}"));

			PseudoTuple ps3 = new PseudoTuple(sp);
			ps3.instantiate(0, new StringConstant("D")).instantiate(1, new StringConstant("D"));
			bte.deriveFact(ps2, program);
			sb.setLength(0);
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(B,A)(C,D)(D,C)}"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testDeriveAll1() {
		try {
			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_7.in"));
			BacktrackingEvaluation bte = new BacktrackingEvaluation();
			bte.evaluate(program, defaultDescr);
			FormalPredicate sp = program.getFormalPredicate(0);
			StringBuilder sb = new StringBuilder();
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(C,D)}"));
			bte.deriveAllFacts(sp, program);
			sb.setLength(0);
			sp.relation.collectRelation(sb);
			assertTrue(sb.toString().equals("[A]{(A,B)(B,A)(C,D)(D,C)}"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	*/

	/**
	 * relationsTest_1.in A("A", "B"). A("B", "C").
	 * 
	 * B(x, y) :- A(y, x).
	 * 
	 * C(x, "A") :- A(x, "B"). C("A", x) :- A("B", x).
	 * 
	 * D(1, x) :- A("A", x). D(2, x) :- A("B", x).
	 */
//	@Test
//	public void testDeriveAll2() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_1.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			FormalPredicate sp = program.getFormalPredicate(0);
//			StringBuilder sb = new StringBuilder();
//			sp.relation.collectRelation(sb);
//			assertTrue(sb.toString().equals("[A]{(A,B)(B,C)}"));
//
//			FormalPredicate spC = program.getFormalPredicate(2);
//			bte.deriveAllFacts(spC, program);
//			sb.setLength(0);
//			spC.relation.collectRelation(sb);
//			assertTrue(sb.toString().equals("[C]{(A,A)(A,C)}"));
//
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}

	/**
	 * relationsTest_8.in A("A", "B"). A("B", "C").
	 * 
	 * B(x, y) :- A(y, x). A(x, y) :- B(x, y).
	 * 
	 * C(x, "A") :- A(x, "B"). C("A", x) :- A("B", x).
	 */
//	@Test
//	public void testDeriveAll3() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_8.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			FormalPredicate sp = program.getFormalPredicate(0);
//			StringBuilder sb = new StringBuilder();
//			sp.relation.collectRelation(sb);
//			assertTrue(sb.toString().equals("[A]{(A,B)(B,C)}"));
//
//			FormalPredicate spC = program.getFormalPredicate(2);
//			bte.deriveAllFacts(spC, program);
//			sb.setLength(0);
//			spC.relation.collectRelation(sb);
//			assertTrue(sb.toString().equals("[C]{(A,A)(A,C)(C,A)}"));
//
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}

	/**
	 * relationsTest_9.in A("A", "B"). A("B", "C"). A(x, z) :- A(x, y), A(y, z).
	 */
//	@Test
//	public void testDeriveAll4() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_9.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			FormalPredicate sp = program.getFormalPredicate(0);
//			StringBuilder sb = new StringBuilder();
//			sp.relation.collectRelation(sb);
//			assertTrue(sb.toString().equals("[A]{(A,B)(B,C)}"));
//
//			bte.deriveAllFacts(sp, program);
//			sb.setLength(0);
//			sp.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[A]{(A,B)(A,C)(B,C)}"));
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}

	/**
	 * relationsTest_11.in C("C11", "C12", "C13"). B(x, x) :- C(x, y, z).
	 */
//	@Test
//	public void testDeriveAll5() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_11.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			StringBuilder sb = new StringBuilder();
//
//			FormalPredicate spB = program.getFormalPredicate(0);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{}"));
//
//			bte.deriveAllFacts(spB, program);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{(C11,C11)}"));
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}

	/**
	 * relationsTest_12.in
	 * 
		A("A", "B").
		B("D", "E").
		C("C11", "C12", "C13").
		
		A(x, y) :- B(y, x).
		B(x, y) :- A(y, x).
		B(x, x) :- C(x, f1, f2).
		B(x, x) :- C(f1, x, f2).
		B(x, x) :- C(f1, f2, x)
	 */
//	@Test
//	public void testDeriveAll6() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_12.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			StringBuilder sb = new StringBuilder();
//			
//			FormalPredicate spA = program.getFormalPredicate(0);
//			sb.setLength(0);
//			spA.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[A]{(A,B)}"));
//
//			FormalPredicate spB = program.getFormalPredicate(1);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{(D,E)}"));
//
//			bte.deriveAllFacts(spA, program);
//			sb.setLength(0);
//			spA.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[A]{(A,B)(C11,C11)(C12,C12)(C13,C13)(E,D)}"));
//			
//			bte.deriveAllFacts(spB, program);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{(B,A)(C11,C11)(C12,C12)(C13,C13)(D,E)}"));
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}
	/*
	A("A", "B").
	B("D", "E").
	C("C11", "C12", "C13").
	C("C21", "C22", "C23").

	A(x, y) :- B(y, x).
	B(x, y) :- A(y, x).
	B(x, y) :- C(x, y, f).
	B(x, y) :- C(f, x, y).
	B(x, y) :- C(x, f, y).
	*/
//	@Test
//	public void testDeriveAll7() {
//		try {
//			Program program = (Program) FileUtil.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_13.in"));
//			BacktrackingEvaluation bte = new BacktrackingEvaluation();
//			bte.evaluate(program, defaultDescr);
//			StringBuilder sb = new StringBuilder();
//			
//			FormalPredicate spA = program.getFormalPredicate(0);
//			sb.setLength(0);
//			spA.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[A]{(A,B)}"));
//
//			FormalPredicate spB = program.getFormalPredicate(1);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{(D,E)}"));
//
//			bte.deriveAllFacts(spA, program);
//			sb.setLength(0);
//			spA.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[A]{(A,B)(C12,C11)(C13,C11)(C13,C12)(C22,C21)(C23,C21)(C23,C22)(E,D)}"));
//			
//			bte.deriveAllFacts(spB, program);
//			sb.setLength(0);
//			spB.relation.collectRelation(sb);
//			System.out.println(sb.toString());
//			assertTrue(sb.toString().equals("[B]{(B,A)(C11,C12)(C11,C13)(C12,C13)(C21,C22)(C21,C23)(C22,C23)(D,E)}"));
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}
    /*
        relationsTest_14.in
        A("A", "B").
        B("D", "E").
        C("C11", "C12", "C13").
        C("C21", "C22", "C23").

        A(x,x) :- C(x,f1,f2).
        A(x, y) :- B(y, x).
        B(x, y) :- C(x,y,f).
        C(x,y,z) :- A(x,y), A(z,z).
    */
	// @Test
	// public void testDeriveAll8() {
	//     try {
	//         Program program = (Program) Util.parse(new File(TEST_DIRECTORY_VALID, "relationsTest_14.in"));
	//         BacktrackingEvaluation bte = new BacktrackingEvaluation();
	//         bte.evaluate(program);
	//         StringBuilder sb = new StringBuilder();
	//
	//         FormalPredicate spA = program.getFormalPredicate(0);
	//         sb.setLength(0);
	//         spA.relation.collectRelation(sb);
	//         System.out.println(sb.toString());
	//         assertTrue(sb.toString().equals("[A]{(A,B)}"));
    //
	//         FormalPredicate spB = program.getFormalPredicate(1);
	//         sb.setLength(0);
	//         spB.relation.collectRelation(sb);
	//         System.out.println(sb.toString());
	//         assertTrue(sb.toString().equals("[B]{(D,E)}"));
	//
	//         FormalPredicate spC = program.getFormalPredicate(2);
	//         sb.setLength(0);
	//         spC.relation.collectRelation(sb);
	//         System.out.println(sb.toString());
	//         assertTrue(sb.toString().equals("[C]{(C11,C12,C13)(C21,C22,C23)}"));
    //
	//         bte.deriveAllFacts(spA, program);
	//         sb.setLength(0);
	//         spA.relation.collectRelation(sb);
	//         System.out.println(sb.toString());
	//         assertTrue(sb.toString().equals("[A]{(A,A)(A,B)(B,A)(B,B)(C11,C11)(C11,C12)(C12,C11)(C12,C12)(C21,C21)(C21,C22)(C22,C21)(C22,C22)(D,D)(D,E)(E,D)(E,E)}"));
	//
	//         bte.deriveAllFacts(spB, program);
	//         sb.setLength(0);
	//         spB.relation.collectRelation(sb);
	//         System.out.println(sb.toString());
	//         assertTrue(sb.toString().equals("[B]{(B,A)(C11,C12)(C11,C13)(C12,C13)(C21,C22)(C21,C23)(C22,C23)(D,E)}"));
	//     } catch (Exception e) {
	//         e.printStackTrace();
	//     }
	// }
}
