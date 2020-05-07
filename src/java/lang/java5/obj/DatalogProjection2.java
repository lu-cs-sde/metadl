package lang.java5.obj;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.time.StopWatch;
import org.extendj.ast.ASTNode;

import org.apache.commons.lang3.tuple.Pair;

import lang.io.SimpleLogger;
import lang.io.StringUID;
import lang.relation.PseudoTuple;
import lang.relation.Relation;

public class DatalogProjection2 {
	private ASTNode<?> root;
	private Map<ASTNode<?>, Integer> nodeNumber = new HashMap<>();

	private int currentNumber = 0;
	private Relation datalogProjection = new Relation(5);

	public DatalogProjection2(ASTNode<?> root) {
		this.root = root;
	}

	private int nodeId(ASTNode<?> n) {
		if (nodeNumber.containsKey(n)) {
			return nodeNumber.get(n);
		} else {
			currentNumber++;
			nodeNumber.put(n, currentNumber);
			return currentNumber;
		}
	}

	public void generate() {
		// traverse the tree
		StopWatch traverseTime = StopWatch.createStarted();
		traverse(root);
		traverseTime.stop();
		StopWatch attributeMapTime = StopWatch.createStarted();
		mapAttributes(Pair.of("type", "ATTR_type"), Pair.of("decl", "ATTR_decl"), Pair.of("genericDecl", "ATTR_generic_decl"));
		attributeMapTime.stop();
		SimpleLogger.logger().time("Object AST initial traversal: " + traverseTime.getTime() + "ms");
		SimpleLogger.logger().time("Attribute tabulation: " + attributeMapTime.getTime() + "ms");
	}

	@SafeVarargs
	private void mapAttributes(Pair<String, String> ...attrs) {
		Set<ASTNode<?>> currentNodes = new HashSet<>(nodeNumber.keySet());
		do {
			// take a snapshot of the already visited nodes
			Set<ASTNode<?>> existingNodes = new HashSet<>(nodeNumber.keySet());

			for (ASTNode<?> n : currentNodes) {
				for (Pair<String, String> attrRelPair : attrs) {
					String attributeName = attrRelPair.getLeft();
					String relName = attrRelPair.getRight();
					ASTNode<?> r = nodeAttribute(n, attributeName);
					if (r == null)
						continue;
					if (!nodeNumber.containsKey(r)) {
						// attributes may refer to NTAs, that were not visited, visit
						// them
						traverse(r);
					}
					datalogProjection.addTuple(makeTuple(relName, n, -1, r, ""));
				}
			}

			// in the traversal of the NTAs we may have encountered new nodes,
			// compute any eventual attributes for them too.
			currentNodes = new HashSet<>(nodeNumber.keySet());
			currentNodes.removeAll(existingNodes);

			// TODO: prevent the analysis from dinving into rt.jar, which
			// takes a long (O(minutes)) time.
			currentNodes.removeIf(n -> isLibraryNode(n));
		} while (!currentNodes.isEmpty());
	}

	private void traverse(ASTNode<?> n) {
		assert n.getNumChild() == n.getNumChildNoTransform();
		for (int i = 0; i < n.getNumChildNoTransform(); ++i) {
			ASTNode<?> childNT = n.getChildNoTransform(i);
			traverse(childNT);
			if (childNT.mayHaveRewrite()) {
				ASTNode<?> child = n.getChild(i);
				if (child != childNT) {
					traverse(child);
					recordRewrittenNode(childNT, child);
				}
			}
		}

		nodeId(n);
		datalogProjection.addTuples(toTuples(n));
		datalogProjection.addTuples(srcLoc(n));
	}

	private PseudoTuple makeTuple(Object ...args) {
		List<lang.ast.Term> elems = new ArrayList<>(args.length);
		for (Object o : args) {
			if (o instanceof ASTNode) {
				elems.add(new lang.ast.IntConstant("" + nodeId((ASTNode<?>)o)));
			} else if (o instanceof String) {
				elems.add(new lang.ast.StringConstant((String)o));
			} else if (o instanceof Integer) {
				elems.add(new lang.ast.IntConstant("" + (Integer)o));
			} else {
				throw new RuntimeException("Can't add an element of type " + o.getClass() + " to a tuple.");
			}
		}

		return new PseudoTuple(elems);
	}

	private void recordRewrittenNode(ASTNode<?> original, ASTNode<?> target) {
		datalogProjection.addTuple(makeTuple("REWRITE", original, -1, target, ""));
	}

	public Relation getRelation() {
		return datalogProjection;
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

	private List<PseudoTuple> srcLoc(ASTNode<?> n) {
		java.util.List<PseudoTuple> ret = new ArrayList<>();
		// record the source location
		{
			// ("SourceInfo, CurrentNodeId, StartLine, StartCol, FileName)
			lang.ast.StringConstant Kind = new lang.ast.StringConstant("SrcLocStart");
			lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + nodeId(n));
			lang.ast.IntConstant Line = new lang.ast.IntConstant("" + beaver.Symbol.getLine(n.getStart()));
			lang.ast.IntConstant Col = new lang.ast.IntConstant("" + beaver.Symbol.getColumn(n.getStart()));
			lang.ast.StringConstant SrcFile = new lang.ast.StringConstant(getSourceFile(n));
			ret.add(new PseudoTuple(Kind, CurrentNodeId, Line, Col, SrcFile));
		}

		{
			// ("SourceInfo, CurrentNodeId, EndLine, EndCol, "")
			lang.ast.StringConstant Kind = new lang.ast.StringConstant("SrcLocEnd");
			lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + nodeId(n));
			lang.ast.IntConstant Line = new lang.ast.IntConstant("" + beaver.Symbol.getLine(n.getEnd()));
			lang.ast.IntConstant Col = new lang.ast.IntConstant("" + beaver.Symbol.getColumn(n.getEnd()));
			// avoid printing the source file once again to avoid bloating the output table
			lang.ast.StringConstant SrcFile = new lang.ast.StringConstant("");
			ret.add(new PseudoTuple(Kind, CurrentNodeId, Line, Col, SrcFile));
		}
		return ret;
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

	private static<T> T nodeAttribute(ASTNode<?> n, String attrName) {
		try {
			Method m = n.getClass().getMethod(attrName);
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
						Method m = nn.getClass().getMethod("getID");
						String id = (String)m.invoke(n);
						assert id != null;
						r.add(id);
					} catch (ReflectiveOperationException e) {
						// do nothing
					}
				});
		return r;
	}

	private List<PseudoTuple> toTuples(ASTNode<?> n) {
		String relName = getRelation(n);
		java.util.List<PseudoTuple> ret = new ArrayList<>();

		// the children in the tree
		int childIndex = 0;
		for (int i = 0; i < n.getNumChildNoTransform(); ++i) {
			ASTNode<?> child = n.getChildNoTransform(i);

			lang.ast.StringConstant Kind = new lang.ast.StringConstant(relName);
			lang.ast.IntConstant ChildId  = new lang.ast.IntConstant("" + nodeId(child));
			lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + nodeId(n));
			lang.ast.IntConstant ChildIdx = new lang.ast.IntConstant("" + childIndex++);
			lang.ast.StringConstant Token = new lang.ast.StringConstant("");
			ret.add(new PseudoTuple(Kind, CurrentNodeId, ChildIdx, ChildId, Token));
		}

		// other tokens attached to the node
		for (String t : tokens(n)) {
			// For every token, we generate two tuples
			// ("NodeKind", CurrentNodeId, ChildIdx, ChildId, "")
			// ("Token", ChildId, 0, 0, "TokenAsString")
			int tokenUID = StringUID.getInstance().uid(t);
			{
				// Add a tuple to the current node relation
				lang.ast.StringConstant Kind = new lang.ast.StringConstant(relName);
				lang.ast.IntConstant ChildId = new lang.ast.IntConstant("" + tokenUID);
				lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + nodeId(n));
				lang.ast.IntConstant ChildIdx = new lang.ast.IntConstant("" + childIndex++);
				lang.ast.StringConstant Token = new lang.ast.StringConstant("");
				ret.add(new PseudoTuple(Kind, CurrentNodeId, ChildIdx, ChildId, Token));
			}

			{
				// Add a tuple to Token relation
				lang.ast.StringConstant Kind = new lang.ast.StringConstant("Terminal");
				lang.ast.IntConstant ChildId = new lang.ast.IntConstant("0");
				lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + tokenUID);
				lang.ast.IntConstant ChildIdx = new lang.ast.IntConstant("0");
				lang.ast.StringConstant Token = new lang.ast.StringConstant(t);
				ret.add(new PseudoTuple(Kind, CurrentNodeId, ChildIdx, ChildId, Token));
			}
		}

		if (childIndex == 0) {
			// This node has no children, emit that
			lang.ast.StringConstant Kind = new lang.ast.StringConstant(relName);
			lang.ast.IntConstant ChildId  = new lang.ast.IntConstant("-1");
			lang.ast.IntConstant CurrentNodeId = new lang.ast.IntConstant("" + nodeId(n));
			lang.ast.IntConstant ChildIdx = new lang.ast.IntConstant("-1");
			lang.ast.StringConstant Token = new lang.ast.StringConstant("");
			ret.add(new PseudoTuple(Kind, CurrentNodeId, ChildIdx, ChildId, Token));
		}

		return ret;
	}
}
