package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.TreeSet;

import org.junit.jupiter.api.Test;

import lang.ast.IntConstant;
import lang.ast.StringConstant;
import lang.ast.Variable;
import lang.relation.Binding;
import lang.relation.Binding.BindOverlap;
import lang.relation.Binding.BindResult;
import lang.relation.PseudoTuple;
import lang.relation.Relation;

public class RelationTests {
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
	public void testBindingCreate() {
		PseudoTuple ps = new PseudoTuple(4).set(0, new Variable("x1")).set(1, new Variable("x2"))
				.instantiate(2, new StringConstant("C")).set(3, new Variable("x1"));
		Binding b = Binding.createBinding(ps);
		assertTrue(b.toString().equals("[(C -> [2]), (x1 -> [0, 3]), (x2 -> [1])]"));
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
		
		System.out.println(b2);
		System.out.println(s.tuples());
		System.out.println(expanded.tuples());
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
				.equals("[(C -> [2]) ++ (C -> [2]), (x -> [0, 3]) ++ (x -> [1]), (y -> [1]) ++ (y -> [0, 3])]"));
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
				.equals("[(C -> [2]) ++ (C -> [2]), (x -> [0, 3]) ++ (x -> [1]), (y -> [1]) ++ (y -> [0, 3])]"));

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
	public void testDifference() {
		Relation r1 = Relation.of(PseudoTuple.of(new StringConstant("A"), new StringConstant("B")),
					PseudoTuple.of(new StringConstant("B"), new StringConstant("C")));
		
		Relation r2 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("B")));
		
		Relation d1 = Relation.difference(r1, r2);
		assertEquals("[(B,C)]", d1.tuples().toString());
		Relation d2 = Relation.difference(r2, r1);
		assertTrue(d2.tuples().isEmpty());
	}

	@Test
	public void testDifferenceWithBinding() {
		
	}

	@Test
	public void testExpand() {
		Relation r1 = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2")),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C")),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4")),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C")));
		
		Relation expected = Relation.of(
				PseudoTuple.of(new StringConstant("A"), new StringConstant("A"), new StringConstant("C"), PseudoTuple.uninitializedVar),
				PseudoTuple.of(new StringConstant("B"), new StringConstant("B"), new StringConstant("C2"), PseudoTuple.uninitializedVar),
				PseudoTuple.of(new StringConstant("C"), new StringConstant("C"), new StringConstant("C"), PseudoTuple.uninitializedVar),
				PseudoTuple.of(new StringConstant("D"), new StringConstant("D"), new StringConstant("C4"), PseudoTuple.uninitializedVar),
				PseudoTuple.of(new StringConstant("E"), new StringConstant("E"), new StringConstant("C"), PseudoTuple.uninitializedVar));
		r1.expand();
		assertTrue(r1.equals(expected));
	}
/*
	@Test
	public void testImmediateConsequence() throws IOException, beaver.Parser.Exception {
		Description descr = FileUtil.parseDescription("internal::bottomupnaive ./tests/evaluation/evalTest_1.in");
		Program program = (Program) FileUtil.parse(new File(descr.getInput().getPath()));
		BottomUpNaiveIterative eval = (BottomUpNaiveIterative)descr.evaluationMethod();
		for(FormalPredicate fp : program.getFormalPredicates()) {
			fp.relation = new Relation(fp.realArity());
		}
		program.getClauseList().forEach(c -> {
			if(c.isRule()) {
				Rule r = (Rule) c;
				eval.immediateConsequence(r);
			}
		});
		System.out.println(program.getClause(0).getHeads(0).predicate().formalpredicate().relation.tuples());
		assertTrue(program.getClause(0).getHeads(0).predicate().formalpredicate().relation.tuples().toString().equals("[(A,B), (A,C), (B,C)]"));
	}
*/
}
