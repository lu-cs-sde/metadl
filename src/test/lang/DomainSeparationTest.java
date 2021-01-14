package lang;



import lang.ast.FormalPredicate;
import lang.ast.FormalPredicateMap;
import lang.ast.Program;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Collections;
import java.util.Set;

import org.junit.jupiter.api.Test;

public class DomainSeparationTest {
	private static Program loadAndCompile(String file) {
		String path = "./tests/domain-separation/" + file;
		CmdLineOpts opts = new CmdLineOpts();
		opts.setAction(CmdLineOpts.Action.CHECK);
		opts.setInputFile(path);

		try {
			Program p = Compiler.parseProgram(opts);
			Compiler.checkProgram(p, opts);
			return p;
		} catch (Exception e) {
			return null;
		}
	}

	@Test public void testLocalPredicates() {
		Program p = loadAndCompile("domain.mdl");
		FormalPredicateMap predMap = p.formalPredicateMap();
		FormalPredicate Name = predMap.get("Name");
		assertTrue(Name.isLocal());
		assertTrue(Name.isASTPredicate());

		FormalPredicate A1 = predMap.get("A1");
		assertFalse(A1.isLocal());
		assertTrue(A1.isASTPredicate());

		FormalPredicate A2 = predMap.get("A2");
		assertFalse(A2.isLocal());
		assertTrue(A2.isASTPredicate());

		FormalPredicate A3 = predMap.get("A3");
		assertTrue(A3.isLocal());
		assertTrue(A3.isASTPredicate());

		FormalPredicate B = predMap.get("B");
		assertFalse(B.isLocal());
		assertTrue(B.isASTPredicate());

		FormalPredicate C = predMap.get("C");
		assertTrue(C.isLocal());
		assertTrue(C.isASTPredicate());

		FormalPredicate Pat = predMap.get("Pat");
		assertTrue(Pat.isLocal());
		assertTrue(Pat.isASTPredicate());
	}

	@Test public void testCircularDeps() {
		Program p = loadAndCompile("circular.mdl");

		FormalPredicateMap predMap = p.formalPredicateMap();
		FormalPredicate B = predMap.get("B");
		FormalPredicate C = predMap.get("C");
		assertEquals(Set.of(Set.of(0, 1)), B.domainSignature().equivalenceSets());
		assertEquals(Set.of(Set.of(0, 1)), C.domainSignature().equivalenceSets());
		assertTrue(B.isASTPredicate());
		assertTrue(C.isASTPredicate());
		// B and C use the DECL attribute, which is global
		assertTrue(B.isLocal());
		assertTrue(C.isLocal());
	}

	@Test public void testLocal1() {
		Program p = loadAndCompile("locals1.mdl");

		FormalPredicateMap predMap = p.formalPredicateMap();
		FormalPredicate P = predMap.get("P");
		assertEquals(P.localTerms(), Collections.emptySet());

		FormalPredicate Q = predMap.get("Q");
		assertEquals(Q.localTerms(), Set.of(0));

		FormalPredicate R = predMap.get("R");
		assertEquals(R.localTerms(), Set.of(2));

		FormalPredicate S = predMap.get("S");
		assertEquals(S.localTerms(), Collections.emptySet());

		FormalPredicate T = predMap.get("T");
		assertEquals(T.localTerms(), Set.of(0, 1));

		FormalPredicate U = predMap.get("U");
		assertEquals(U.localTerms(), Set.of(0));

		FormalPredicate V = predMap.get("V");
		assertEquals(V.localTerms(), Collections.emptySet());

		FormalPredicate W = predMap.get("W");
		assertEquals(W.localTerms(), Set.of(0));
	}
}
