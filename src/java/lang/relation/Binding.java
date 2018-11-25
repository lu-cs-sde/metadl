package lang.relation;

import java.util.TreeMap;
import java.util.TreeSet;

import lang.ast.Term;
import lang.relation.Binding.BindResult.Direction;
import lang.relation.Binding.BindResult.TaggedBind;
import lang.relation.Binding.BindTerm;

@SuppressWarnings("serial")
public class Binding extends TreeSet<BindTerm> {

	public static Binding anyBinding = new Binding();

	public boolean satisfiedBy(PseudoTuple t) {
		for (BindTerm b : this) {
			PseudoTuple proj = t.project(b.coords);
			if (proj.size == 0)
				return false;
			if (!proj.isUniInfo())
				return false;
			if (!b.t.isVariable() && Term.termComparator.compare(proj.coord(0), b.t) != 0)
				return false;
		}
		return true;
	}
	
	public int totalSize() {
		int size = 0;
		for(BindTerm bt : this) {
			size += bt.coords.size();
		}
		return size;
	}

	public static void bind(Binding binding, Term t, int index) {
		BindTerm bind = new BindTerm(t);
		BindTerm bindExisting = binding.floor(bind);
		if (bindExisting != null && bindExisting.equals(bind)) {
			bindExisting.coords.add(index);
		} else {
			bind.coords = new TreeSet<>();
			bind.coords.add(index);
			binding.add(bind);
		}
	}

	public static Binding createBinding(PseudoTuple t) {
		Binding binding = new Binding();
		for (int i = 0; i != t.size; ++i) {
			bind(binding, t.coord(i), i);
		}
		return binding;
	}

	public static Binding of(BindTerm... binds) {
		Binding binding = new Binding();
		for (BindTerm b : binds)
			binding.add(b);
		return binding;
	}

	public static TreeSet<BindOverlap> intersect(Binding b1, Binding b2) {
		TreeSet<BindOverlap> b = new TreeSet<BindOverlap>();
		b1.forEach(bind_b1 -> {
			BindTerm bind_b2 = b2.floor(bind_b1);
			if (bind_b2 != null && bind_b1.equals(bind_b2)) {
				b.add(new BindOverlap(bind_b1, bind_b2));
			}
		});
		return b;
	}
	
	public static Binding difference(TreeSet<BindOverlap> bo, Binding b) {
		Binding bd = new Binding();
		b.forEach(bt -> {
			if(!bo.contains(new BindOverlap(bt, null))) {
				bd.add(bt);
			}
		});
		return bd;
	}

	private static int add_merge(Binding b_merged, Binding b, int index, TreeMap<BindTerm, TaggedBind> f, Direction d) {
		for (BindTerm bt : b) {
			if (!b_merged.contains(bt) && bt.t.isVariable()) { // Drop Constants since they matter no more
				BindTerm bt_new = new BindTerm(bt.t);
				bt_new.coords.add(index++);
				b_merged.add(bt_new);
				f.put(bt_new, new TaggedBind(d, bt));
			}
		}
		return index;
	}

	public static BindResult merge(TreeSet<BindOverlap> b, Binding b1, Binding b2) {
		Binding b_merged = new Binding();
		TreeMap<BindTerm, TaggedBind> f = new TreeMap<>();

		int index = 0;
		for (BindOverlap bo : b) {
			if (bo.b1.t.isVariable()) { // Drop Constants since they matter no more
				BindTerm bt = new BindTerm(bo.b1.t);
				bt.coords.add(index++);
				b_merged.add(bt);
				f.put(bt, new TaggedBind(Direction.Both, bo));
			}
		}
		index = add_merge(b_merged, b1, index, f, Direction.Left);
		add_merge(b_merged, b2, index, f, Direction.Right);
		return new BindResult(b_merged, f);
	}

	public interface Bind {
	}

	public static class BindResult {
		public Binding b_merged;
		public TreeMap<BindTerm, TaggedBind> f;

		public enum Direction {
			Left, Right, Both
		}

		public BindResult(Binding b_merged, TreeMap<BindTerm, TaggedBind> f) {
			this.b_merged = b_merged;
			this.f = f;
		}

		public static class TaggedBind {
			public Direction d;
			public Bind b;

			public TaggedBind(Direction d, Bind b) {
				this.d = d;
				this.b = b;
			}

			@Override
			public String toString() {
				return b.toString() + " " + d.toString();
			}
		}
	}

	public static class BindOverlap implements Comparable<BindOverlap>, Bind {
		public BindTerm b1;
		public BindTerm b2;

		@Override
		public String toString() {
			return b1.toString() + " ++ " + b2.toString();
		}

		public BindOverlap(BindTerm b1, BindTerm b2) {
			this.b1 = b1;
			this.b2 = b2;
		}

		@Override
		public int compareTo(BindOverlap arg0) {
			return b1.compareTo(arg0.b1);
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof BindOverlap))
				return false;
			return compareTo((BindOverlap) obj) == 0;
		}
	}

	public static class BindTerm implements Comparable<BindTerm>, Bind {
		public TreeSet<Integer> coords = new TreeSet<>();
		public Term t;

		public BindTerm(Term t) {
			this.t = t;
		}

		@Override
		public int compareTo(BindTerm arg0) {
			return Term.termComparator.compare(t, ((BindTerm) arg0).t);
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof BindTerm))
				return false;
			return compareTo((BindTerm) obj) == 0;
		}

		@Override
		public String toString() {
			return "(" + t + " -> " + coords + ")";
		}
	}
}
