package lang.java.obj;

import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.text.StringEscapeUtils;

import org.extendj.ast.ASTNode;
import org.extendj.ast.CompilationUnit;
import org.junit.jupiter.engine.discovery.predicates.IsNestedTestClass;
import org.extendj.ProvenanceStackMachine;
import org.extendj.ast.FileIdStorage;
import org.extendj.ast.Program;

import lang.io.SimpleLogger;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;

public class DatalogProjection2 {
	private long NTANum = 1;
	private FileIdStorage fileIdDb;
	private ProvenanceStackMachine prov;

	// TODO: It's just easier to add environment variable than command-line arguments.
	// This should be removed once we move to a traditional command line parser,
	// that is easier to modify.
	private boolean deepAnalysis = System.getenv().get("METADL_ANALYSIS") != null;

	public DatalogProjection2(FileIdStorage fileIdStorage,
							  ProvenanceStackMachine prov) {
		this.fileIdDb = fileIdStorage;
		this.prov = prov;
	}

	private Map<ASTNode, Long> nodeToId = new HashMap<>();
	private long nodeId(ASTNode<?> n) {
		// if (n.javaDLNodeId >= 0)
		// 	return n.javaDLNodeId;
 		// TODO: consider caching this on the node itself
		Long id = nodeToId.get(n);
		if (id != null) {
			n.javaDLNodeId = id;
			return id;
		}

		if (!n.numberOnDemand(fileIdDb, nodeToId)) {
			// throw new RuntimeException("Node numbering failed.");
			// System.out.print("Do NTA node numbering at " + n + " from " + NTANum);
			NTANum = n.doNodeNumberingNTA(NTANum, nodeToId);
			// System.out.println(" until " + NTANum);
		}

		id = nodeToId.get(n);
		n.javaDLNodeId = id;
		return id;
	}

	private static int NTA_FILE_ID_BIT = 1 << 30; // use bit 30 to get positive IDs
	private long NTAFileId(long fileId) {
		return (((long) (fileId)) | NTA_FILE_ID_BIT) << 32;
	}

	public Set<CompilationUnit> generate(CompilationUnit cu, DatalogProjectionSink tupleSink) {
		NTANum = NTAFileId(fileIdDb.getIdForFile(cu.getClassSource().relativeName()));

		Set<CompilationUnit> externalCUs = traverseAndMapAttributes(cu, cu, tupleSink, "type", "decl", "genericDecl");

		tupleSink.getAST().done();
		tupleSink.getProvenance().done();
		tupleSink.getSrcLoc().done();
		tupleSink.getAttributes().done();
		tupleSink.getNTA().done();

		return externalCUs;
	}

	public void generate(Program p, DatalogProjectionSink tupleSink) {
		Set<CompilationUnit> worklist = new LinkedHashSet<>();
		for (CompilationUnit cu : p.getCompilationUnits()) {
			worklist.addAll(generate(cu, tupleSink));
		}

		while (!worklist.isEmpty()) {
			// pop a CU from the set
			CompilationUnit cu = worklist.iterator().next();
			worklist.remove(cu);
			worklist.addAll(generate(cu, tupleSink));
		}
	}

	private Set<CompilationUnit> traverseAndMapAttributes(CompilationUnit currentCU,
														 ASTNode<?> n,
														 DatalogProjectionSink tupleSink,
														 String ...attrs) {

		boolean mapAttributes = !isLibraryNode(currentCU) || deepAnalysis;

		Queue<ASTNode<?>> currentCUNodes = new ArrayDeque<>();
		Queue<ASTNode<?>> NTANodes = new ArrayDeque<>();

		Set<ASTNode<?>> processed = new HashSet<>();
		Set<CompilationUnit> otherCompilationUnits = new LinkedHashSet<>();

		// seed the worklist
		currentCUNodes.add(n);

		// run the worklist
		while (!(currentCUNodes.isEmpty() && NTANodes.isEmpty())) {
			while (!currentCUNodes.isEmpty()) {
				ASTNode<?> q = currentCUNodes.poll();
				if (processed.contains(q)) {
					continue;
				}
				// mark visited
				processed.add(q);
				// add the tuples to the relation
				toTuples(q, tupleSink.getAST());
				// record source location
				srcLoc(q, tupleSink.getSrcLoc());
				// now process the attributes
				if (mapAttributes) {
					mapAttributes2(q, currentCU, otherCompilationUnits, currentCUNodes, NTANodes, tupleSink.getAttributes(),
								   tupleSink.getProvenance(), attrs);
				}
				// add the children to the worklist
				for (int i = 0; i < q.getNumChildNoTransform(); ++i) {
					ASTNode<?> childNT = q.getChildNoTransform(i);
					// TODO: ExtendJ sometimes returns null for childNT. Understand why.
					if (childNT != null) {
						currentCUNodes.add(childNT);
						if (childNT.mayHaveRewrite()) {
							ASTNode<?> child = q.getChild(i);
							if (child != childNT) {
								currentCUNodes.add(child);
								recordRewrittenNode(childNT, child, tupleSink.getAttributes());
							}
						}
					}
				}
			}

			while (!NTANodes.isEmpty()) {
				ASTNode<?> q = NTANodes.poll();
				if (processed.contains(q)) {
					continue;
				}
				// mark visited
				processed.add(q);
				// add the tuples to the relation
				toTuples(q, tupleSink.getNTA());
				// now process the attributes
				if (mapAttributes) {
					mapAttributes2(q, currentCU, otherCompilationUnits, currentCUNodes, NTANodes, tupleSink.getAttributes(),
								   tupleSink.getProvenance(), attrs);
				}
				// add the children to the worklist
				for (int i = 0; i < q.getNumChildNoTransform(); ++i) {
					ASTNode<?> childNT = q.getChildNoTransform(i);
					// TODO: ExtendJ sometimes returns null for childNT. Understand why.
					if (childNT != null) {
						NTANodes.add(childNT);
						if (childNT.mayHaveRewrite()) {
							ASTNode<?> child = q.getChild(i);
							if (child != childNT) {
								NTANodes.add(child);
								recordRewrittenNode(childNT, child, tupleSink.getAttributes());
							}
						}
					}
				}
			}
		}
		return otherCompilationUnits;
	}

	private void mapAttributes2(ASTNode<?> n,
								CompilationUnit currentCU,
								Set<CompilationUnit> otherCUs,
								Queue<ASTNode<?>> currentCUNodes,
								Queue<ASTNode<?>> ntaNodes,
								TupleInserter attributes,
								TupleInserter attributeProvenance,
								String ...attrs) {
		for (String attrName : attrs) {
			ASTNode<?> r = nodeAttribute(n, attrName);

			if (r == null)
				continue;

			// log the attribute's provenance information
			Set<String> srcs = attrProvenance(n, attrName);
			for (String src : srcs) {
				attributeProvenance.insertTuple(nodeId(n), attrName, src);
			}

			attributes.insertTuple(attrName, nodeId(n), nodeId(r));

			if (r.parentCompilationUnit() == null) {
				// this is an NTA
				ntaNodes.add(r);
			} else if (r.parentCompilationUnit() == currentCU) {
				// this is another node from the current compilation unit
				currentCUNodes.add(r);
			} else {
				otherCUs.add(r.parentCompilationUnit());
			}
		}
	}

	private void recordRewrittenNode(ASTNode<?> original, ASTNode<?> target, TupleInserter attributes) {
		attributes.insertTuple("rewriteTo", nodeId(original), nodeId(target));
	}

	private static String getRelation(ASTNode<?> n) {
		String nodeName = n.getClass().getName();
		String[] splitNodeName = nodeName.split("\\.");
		String relName = splitNodeName[splitNodeName.length - 1];
		return relName;
	}

	private static boolean isLibraryNode(ASTNode<?> n) {
		String src = n.sourceFile();
		if (src.endsWith(".class"))
			return true;
		return false;
	}

	private void srcLoc(ASTNode<?> n, TupleInserter srcLoc) {
		long nid = nodeId(n);
		String srcFile = n.sourceFile();
		srcLoc.insertTuple(nid,
						   beaver.Symbol.getLine(n.getStart()),
						   beaver.Symbol.getColumn(n.getStart()),
						   beaver.Symbol.getLine(n.getEnd()),
						   beaver.Symbol.getColumn(n.getEnd()),
						   srcFile);
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
		if (prov == null)
			return Collections.emptySet();

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
		s = s.replace('[', '(').replace(']', ')').replace('\'', '_').replace('"', '_').replace('\n', '_').replace(',', '_');
		 // return StringEscapeUtils.escapeCsv(s);
		 return s;
	}

	private int tokenUID = Integer.MAX_VALUE;
	private long freshTokenId(ASTNode<?> n) {
		// give a token id that is file specific
		long fileId = nodeId(n) >> 32;
		tokenUID--;
		return (fileId << 32) | tokenUID;
	}

	private void toTuples(ASTNode<?> n, TupleInserter astTupleSink) {
		long nid = nodeId(n);
		String relName = getRelation(n);

		// the children in the tree
		int childIndex = 0;
		for (int i = 0; i < n.getNumChildNoTransform(); ++i) {
			ASTNode<?> child = n.getChildNoTransform(i);
			// TODO: ExtendJ sometimes returns null for child. Understand why.
			if (child != null) {
				astTupleSink.insertTuple(relName, nodeId(n), childIndex, nodeId(child), "");
				if (child.mayHaveRewrite()) {
					ASTNode<?> childT = n.getFrozenChild(i);
					astTupleSink.insertTuple(relName, nodeId(n), childIndex, nodeId(childT), "");
				}
			}
			childIndex++;
		}

		// other tokens attached to the node
		for (String t : tokens(n)) {
			// For every token, we generate two tuples
			// ("NodeKind", CurrentNodeId, ChildIdx, ChildId, "")
			// ("Token", ChildId, 0, 0, "TokenAsString")
			long tid = freshTokenId(n);
			// Add a tuple to the current node relation
			astTupleSink.insertTuple(relName, nid, childIndex++, tid, "");
			// Add a tuple to Token relation
			astTupleSink.insertTuple("Terminal", tid, 0, 0, cleanTerminal(t));
		}

		if (childIndex == 0) {
			// This node has no children, emit that
			astTupleSink.insertTuple(relName, nid, -1, -1, "");
		}
	}
}
