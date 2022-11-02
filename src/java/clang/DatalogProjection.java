package clang;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.commons.lang3.tuple.Pair;

import lang.c.obj.ast.ASTNode;
import lang.c.obj.ast.ASTNodeAnnotation;

import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;

public class DatalogProjection {
	private final FileIdDatabase fidStore;
	private final  DatalogProjectionSink sink;
	private final ASTImporter astImporter = new ASTImporter();

	public DatalogProjection(FileIdDatabase fidStore, DatalogProjectionSink sink) {
		this.fidStore = fidStore;
		this.sink = sink;
	}

	public void project(String file) throws IOException {
		final ASTTranslator astTranslator = new ASTTranslator();
		AST.Node root = astImporter.importAST(file);
		ASTNode tRoot = astTranslator.translate(root);

		int fileId = fidStore.getIdForFile(file);
		toTuples(tRoot, fileId, new MutableInt());
	}

	private static Map<Class<?>, java.util.List<Pair<String, Method>>> tokenAccessors = new HashMap<>();

	private static java.util.List<Pair<String, Method>> getTokenAccessors(ASTNode<?> n) {
		java.util.List<Pair<String, Method>> tokenAccessor = tokenAccessors.get(n.getClass());
		if (tokenAccessor != null)
			return tokenAccessor;

		for (Method method : n.getClass().getMethods()) {
			ASTNodeAnnotation.Token token = method.getAnnotation(ASTNodeAnnotation.Token.class);
			if (token != null && method.getReturnType().equals(String.class)) {
				if (tokenAccessor == null) {
					tokenAccessor = new ArrayList<>();
				}
				tokenAccessor.add(Pair.of(token.name(), method));
			}
		}

		if (tokenAccessor == null) {
			tokenAccessor = Collections.emptyList();
		}

		tokenAccessors.put(n.getClass(), tokenAccessor);

		return tokenAccessor;
	}

	public static java.util.List<Pair<String, String>> namedTokens(ASTNode<?> n) {
		java.util.List<Pair<String, String>> tokens = new ArrayList<>();
		for (Pair<String, Method> accessor : getTokenAccessors(n)) {
			try {
				tokens.add(Pair.of(accessor.getLeft(), (String) accessor.getRight().invoke(n)));
			} catch (InvocationTargetException e) {
				throw new RuntimeException(e);
			} catch (IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		}
		return tokens;
	}

	private Map<ASTNode, Integer> nodeId = new HashMap<>();

	private long makeFullId(int nodeId, int fileId) {
		return ((long) fileId) << 32 | (long) nodeId;
	}

	private int nodeId(ASTNode<?> n) {
		Integer nid = nodeId.get(n);
		if (nid == null) {
			throw new RuntimeException("Node should have been visited before calling nodeId.");
		}
		return nid;
	}

	private void toTuples(ASTNode<?> n, int fileId, MutableInt runningNodeId) {
		String relName = n.getClass().getSimpleName();

		// traverse the children first
		for (int i = 0; i < n.getNumChild(); ++i) {
			toTuples(n.getChild(i), fileId, runningNodeId);
		}

		int currentNodeId = runningNodeId.getAndIncrement();
		nodeId.put(n, currentNodeId);

		// in principle, these should be traversed before assigning a node id
		// to the current node; but since these are just tokens, it doesn't really matter
		int childIndex = n.getNumChild();
		for (Pair<String, String> t : namedTokens(n)) {
			int tokenId = runningNodeId.getAndIncrement();
			sink.getAST().insertTuple(relName, makeFullId(currentNodeId, fileId), childIndex++, makeFullId(tokenId, fileId), "");
			sink.getAST().insertTuple(t.getLeft(), makeFullId(tokenId, fileId), 0, 0, t.getRight());
		}

		for (int i = 0; i < n.getNumChild(); ++i) {
			sink.getAST().insertTuple(relName, makeFullId(currentNodeId, fileId), i, makeFullId(nodeId(n.getChild(i)), fileId), "");
		}

		if (namedTokens(n).isEmpty() && n.getNumChild() == 0) {
			sink.getAST().insertTuple(relName, makeFullId(currentNodeId, fileId), -1, -1, "");
		}
	}
}
