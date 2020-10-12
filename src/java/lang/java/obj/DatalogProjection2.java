package lang.java.obj;

import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;

import org.extendj.ast.ASTNode;
import org.extendj.ProvenanceStackMachine;

import lang.io.SimpleLogger;
import lang.io.StringUID;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;

public class DatalogProjection2 {
	private ASTNode<?> root;

	private int currentNumber = 0;
	private TupleInserter programRepresentation;
	private TupleInserter attributeProvenance;
	private int tokenUID = Integer.MAX_VALUE;
	private Map<ASTNode<?>, Integer> nodeNumber = new HashMap<>();
	private Map<Integer, ASTNode<?>> number2Node = new HashMap<>();


	// TODO: It's just easier to add environment variable than command-line arguments.
	// This should be removed once we move to a traditional command line parser,
	// that is easier to modify.
	private boolean deepAnalysis = System.getenv().get("METADL_ANALYSIS") != null;

	public DatalogProjection2(ASTNode<?> root,
							  DatalogProjectionSink tupleSink) {
		this.root = root;
		this.programRepresentation = tupleSink.getAst();
		this.attributeProvenance = tupleSink.getProvenance();
	}

	private int nodeId(ASTNode<?> n) {
		if (visited(n)) {
			return nodeNumber.get(n);
		} else {
			 currentNumber++;
			 nodeNumber.put(n, currentNumber);
			 number2Node.put(currentNumber, n);
			 return currentNumber;
		}
	}

	private boolean visited(ASTNode<?> n) {
		return nodeNumber.containsKey(n);
	}

	public void generate() {
		// traverse the tree
		StopWatch traverseTime = StopWatch.createStarted();
		Queue<ASTNode<?>> worklist = new ArrayDeque<>();
		traverse(root, worklist);
		traverseTime.stop();
		StopWatch attributeMapTime = StopWatch.createStarted();
		mapAttributes(worklist, Pair.of("type", "ATTR_type"), Pair.of("decl", "ATTR_decl"), Pair.of("genericDecl", "ATTR_generic_decl"));
		programRepresentation.done();
		attributeProvenance.done();
		attributeMapTime.stop();
		SimpleLogger.logger().time("Object AST initial traversal: " + traverseTime.getTime() + "ms");
		SimpleLogger.logger().time("Attribute tabulation: " + attributeMapTime.getTime() + "ms");
	}


	final private void mapAttributes(Queue<ASTNode<?>> worklist, Pair<String, String> ...attrs) {
		while (!worklist.isEmpty()) {
			ASTNode<?> n = worklist.poll();
			assert visited(n);

			// prevent the analysis from dinving into rt.jar, which
			// takes a long (O(minutes)) time.
			if (!deepAnalysis && isLibraryNode(n))
				continue;

			for (Pair<String, String> attrRelPair : attrs) {
				String attributeName = attrRelPair.getLeft();
				String relName = attrRelPair.getRight();
				ASTNode<?> r = nodeAttribute(n, attributeName);

				if (r == null)
					continue;

				// log the attribute's provenance information
				Set<String> srcs = attrProvenance(n, attributeName);
				for (String src : srcs) {
					attributeProvenance.insertTuple(nodeId(n), relName, src);
				}

				if (!visited(r)) {
					// attributes may refer to NTAs, that were not visited in the
					// initial traversal, visit them now and every node in the
					// subtree to the worklist, for attribute evaluation
					traverse(r, worklist);
				}
				programRepresentation.insertTuple(relName, nodeId(n), -1, nodeId(r), "");
			}
		}
	}

	private void traverse(ASTNode<?> n, Queue<ASTNode<?>> worklist) {
		worklist.add(n);
		assert n.getNumChild() == n.getNumChildNoTransform();
		for (int i = 0; i < n.getNumChildNoTransform(); ++i) {
			ASTNode<?> childNT = n.getChildNoTransform(i);
			// TODO: ExtendJ sometimes returns null for childNT. Understand why.
			if (childNT != null) {
				traverse(childNT, worklist);
				if (childNT.mayHaveRewrite()) {
					ASTNode<?> child = n.getChild(i);
					if (child != childNT) {
						traverse(child, worklist);
						recordRewrittenNode(childNT, child);
					}
				}
			}
		}

		int nid = nodeId(n);
		toTuples(n, nid);
		srcLoc(n, nid);
	}

	private void recordRewrittenNode(ASTNode<?> original, ASTNode<?> target) {
		programRepresentation.insertTuple("REWRITE", nodeId(original), -1, nodeId(target), "");
	}

	private static String getRelation(ASTNode<?> n) {
		String nodeName = n.getClass().getName();
		String[] splitNodeName = nodeName.split("\\.");
		String relName = splitNodeName[splitNodeName.length - 1];
		return relName;
	}

	private static boolean isLibraryNode(ASTNode<?> n) {
		String src = getSourceFile(n);
		if (src.endsWith(".class"))
			return true;
		return false;
	}

	private static String getSourceFile(ASTNode<?> n) {
		// TODO: ExtendJ does not expose the source file as a public method,
		// but only the source location, which concatentates the file name
		// and the line number.
		String[] loc =  n.sourceLocation().split(":");
		return loc[0];
	}

	private void srcLoc(ASTNode<?> n, int nid) {
		String srcFile = getSourceFile(n);
		programRepresentation.insertTuple("SrcLocStart",
						 nid,
						 beaver.Symbol.getLine(n.getStart()),
						 beaver.Symbol.getColumn(n.getStart()),
						 srcFile);
		programRepresentation.insertTuple("SrcLocEnd",
						 nid,
						 beaver.Symbol.getLine(n.getEnd()),
						 beaver.Symbol.getColumn(n.getEnd()),
						 "");
	}


	private Map<Pair<Class<?>, String>, Method>  reflectCache = new HashMap<>();
	private Set<Pair<Class<?>, String>> noSuchMethodCache = new HashSet<>();
	private Method getMethodForClass(Class<?> c, String name) {
		if (noSuchMethodCache.contains(Pair.of(c, name)))
			return null;
		Method m = reflectCache.get(Pair.of(c, name));
		if (m != null)
			return m;
		try {
			m = c.getMethod(name);
			reflectCache.put(Pair.of(c, name), m);
			return m;
		} catch (NoSuchMethodException e) {
			noSuchMethodCache.add(Pair.of(c, name));
			return null;
		}
	}


	/**
	   Helper class to match the type of the object and apply
	   an action.
	 */
	private static class CastWrapper {
		private Object o;
		CastWrapper(Object o) {
			this.o = o;
		}
		<T> CastWrapper bind(Consumer<T> f) {
			if (o == null)
				return this;
			try {
				f.accept((T)o);
				return new CastWrapper(null);
			} catch (ClassCastException e) {
				return this;
			}
		}
	}

	private <T> T nodeAttribute(ASTNode<?> n, String attrName) {
		try {
			Method m = getMethodForClass(n.getClass(), attrName);
			if (m == null)
				return null;
			Object o = m.invoke(n);
			if (o == null)
				return null;
			return (T)o;
		} catch (ReflectiveOperationException e) {
			// do nothing, the node may be missing the attribute
			return null;
		} catch (ClassCastException e) {
			return null;
		}
	}

	private Set<String> attrProvenance(ASTNode<?> n, String attrName) {
		ProvenanceStackMachine prov = ((org.extendj.ast.Program) root).provenance;
		Set<String> res =  prov.getSources(n, attrName + "()");
		prov.reset();
		return res;
	}

	private List<String> tokens(ASTNode<?> n) {
		ArrayList<String> r = new ArrayList<>();

		CastWrapper w = new CastWrapper(n);
		w.bind((org.extendj.ast.Literal l) -> r.add(l.getLITERAL()))
			.bind((org.extendj.ast.Literal l) -> r.add(l.getLITERAL()))
			.bind((org.extendj.ast.CompilationUnit l) -> r.add(l.getPackageDecl()))
			.bind((org.extendj.ast.PackageAccess l) -> r.add(l.getPackage()))
			.bind((org.extendj.ast.IdUse id) -> r.add(id.getID()))
			.bind((org.extendj.ast.Modifier m) -> r.add(m.getID()))
			.bind((org.extendj.ast.LabeledStmt s) -> r.add(s.getLabel()))
			.bind((org.extendj.ast.BreakStmt b) -> r.add(b.getLabel()))
			.bind((org.extendj.ast.ContinueStmt c) -> r.add(c.getLabel()))
			.bind((org.extendj.ast.ParseName p) -> {
					// TODO: In ExtendJ ParseName.nameParts is not accessible; resort to
					// spliting the string as a workaround
					String[] parts = p.name().split("\\.");
					r.addAll(Arrays.asList(parts));
				})
			.bind((org.extendj.ast.ASTNode nn) -> {
					try {
						Method m = getMethodForClass(nn.getClass(), "getID");
						if (m == null) {
							// do nothing
						} else {
							String id = (String)m.invoke(n);
							assert id != null;
							r.add(id);
						}
					} catch (ReflectiveOperationException e) {
						// do nothing
					}
				});
		return r;
	}

	private String cleanTerminal(String s) {
		// TODO: Souffle assumes that [ or ] are used in the CSV
		// for encoding a record.
		return s.replace('[', '(').replace(']', ')').replace('\'', '_').replace('"', '_').replace('\n', '_');
	}

	private void toTuples(ASTNode<?> n, int nid) {
		String relName = getRelation(n);

		// the children in the tree
		int childIndex = 0;
		for (int i = 0; i < n.getNumChildNoTransform(); ++i) {
			ASTNode<?> child = n.getChildNoTransform(i);
			// TODO: ExtendJ sometimes returns null for child. Understand why.
			if (child != null) {
				programRepresentation.insertTuple(relName, nodeId(n), childIndex, nodeId(child), "");
				if (child.mayHaveRewrite()) {
					ASTNode<?> childT = n.getChild(i);
					programRepresentation.insertTuple(relName, nodeId(n), childIndex, nodeId(childT), "");
				}
			}
			childIndex++;
		}

		// other tokens attached to the node
		for (String t : tokens(n)) {
			// For every token, we generate two tuples
			// ("NodeKind", CurrentNodeId, ChildIdx, ChildId, "")
			// ("Token", ChildId, 0, 0, "TokenAsString")

			// Add a tuple to the current node relation
			programRepresentation.insertTuple(relName, nid, childIndex++, tokenUID, "");
			// Add a tuple to Token relation
			programRepresentation.insertTuple("Terminal", tokenUID, 0, 0, cleanTerminal(t));

			tokenUID--;
		}

		if (childIndex == 0) {
			// This node has no children, emit that
			programRepresentation.insertTuple(relName, nid, -1, -1, "");
		}
	}
}
