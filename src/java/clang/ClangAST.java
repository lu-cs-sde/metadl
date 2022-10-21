package clang;

import java.io.PrintStream;
import java.util.Collections;
import java.util.Iterator;

import org.apache.commons.collections4.iterators.ArrayIterator;
import org.apache.commons.lang3.mutable.MutableObject;

public class ClangAST {

	public static class Loc {
		public int line;
		public int col;
		public String file = "<UNKNOWN>";

		public Loc patch(Loc l) {
			if (l == null)
				return this;
			if (line == 0)
				line = l.line;
			if (col == 0)
				col = l.col;
			if (file == null || file.equals("<UNKNOWN>"))
				file = l.file;
			return this;
		}

		public Loc(Loc l) {
			this.line = l.line;
			this.col = l.col;
			this.file = l.file;
		}
	}

	public static class Range {
		public Loc begin;
		public Loc end;
	}

	public static class Node {
		public String id;
		public String kind;
		public Loc loc;
		public Range range;
		public Node[] inner;
		public String name;

		public void prettyPrint(PrintStream ps) {
			prettyPrintInternal(0, ps);
		}

		public Iterator<Node> children() {
			if (inner == null)
				return Collections.emptyIterator();
			return new ArrayIterator<Node>(inner);
		}

		private void prettyPrintInternal(int offset, PrintStream ps) {
			for (int i = 0; i < offset; ++i) {
				ps.print("  ");
			}

			if (loc != null) {
				ps.printf("<%s> %s %s[%s, %d:%d-%d:%d]\n", id, kind, name != null ? "'" + name + "' " : "", loc.file,
						  range.begin.line, range.begin.col, range.end.line, range.end.col);
			} else {
				ps.printf("<%s> %s %s[BUILTIN]\n", id, kind, name != null ? "'" + name + "' " : "");
			}

			children().forEachRemaining(c -> c.prettyPrintInternal(offset + 1, ps));
		}

		private void patchLocationsInternal(MutableObject<Loc> prev) {
			if (loc != null)
				prev.setValue(loc.patch(prev.getValue()));
			else
				loc = new Loc(prev.getValue());

			if (range != null) {
				if (range.begin != null)
					range.begin.patch(prev.getValue());
				else
					range.begin = new Loc(prev.getValue());

				prev.setValue(range.begin);

				if (range.end != null)
					range.end.patch(prev.getValue());
				else
					range.end = new Loc(range.begin);

				prev.setValue(range.end);
			} else {
				range = new Range();
				range.begin = new Loc(prev.getValue());
				range.end = new Loc(prev.getValue());
			}

			Iterator<Node> c = children();
			while (c.hasNext()) {
				c.next().patchLocationsInternal(prev);
			}
		}

		public void patchLocations() {
			patchLocationsInternal(new MutableObject<>());
		}
	}
}
