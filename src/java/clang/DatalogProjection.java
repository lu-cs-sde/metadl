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

		// tRoot.debugPrint(System.out);

		traverse(tRoot, new MutableInt(), new MutableInt());
		toTuples(file);
		postNumber.clear();
		preNumber.clear();
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

	public static class CNodeIdDesc {
		public static final int FILE_ID_SIZE = 16;
		public static final int PRE_ORD_NUM_SIZE = 24;
		public static final int POST_ORD_NUM_SIZE = 24;

		public static final int POST_ORD_NUM_OFFSET = 0;
		public static final int PRE_ORD_NUM_OFFSET = POST_ORD_NUM_OFFSET + POST_ORD_NUM_SIZE;
		public static final int FILE_ID_OFFSET = PRE_ORD_NUM_OFFSET + PRE_ORD_NUM_SIZE;

		public static long make(long pre, long post, long fileId) {
			assert fileId < (1 << 16); // max 2^16-1 files (a.t.m. gcc and the linux kernel contain less C files)
			assert post < (1 << 24); // max 2^24-1 nodes
			assert pre < (1 << 24);
			return (fileId << CNodeIdDesc.FILE_ID_OFFSET) | (pre << CNodeIdDesc.PRE_ORD_NUM_OFFSET) | (post << CNodeIdDesc.POST_ORD_NUM_OFFSET);
		}

		public static long fileId(long id) {
			return (id >>> FILE_ID_OFFSET) & ((1l << FILE_ID_SIZE) - 1);
		}

		public static long preOrdNum(long id) {
			return (id >>> PRE_ORD_NUM_OFFSET) & ((1l << PRE_ORD_NUM_SIZE) - 1);
		}

		public static long postOrdNum(long id) {
			return (id >>> POST_ORD_NUM_OFFSET) & ((1l << POST_ORD_NUM_SIZE) - 1);
		}
	}

	private long makeFullId(long pre, long post, long fileId) {
		assert fileId < (1 << 16); // max 2^16-1 files (a.t.m. gcc and the linux kernel contain less C files)
		assert post < (1 << 24); // max 2^24-1 nodes
		assert pre < (1 << 24);
		return (fileId << CNodeIdDesc.FILE_ID_OFFSET) | (pre << CNodeIdDesc.PRE_ORD_NUM_OFFSET) | (post << CNodeIdDesc.POST_ORD_NUM_OFFSET);
	}

	private Map<ASTNode, Integer> postNumber = new HashMap<>();
	private Map<ASTNode, Integer> preNumber = new HashMap<>();

	private void traverse(ASTNode<?> n, MutableInt preCounter, MutableInt postCounter) {
		int numTokens = getTokenAccessors(n).size();
		preNumber.put(n, preCounter.getAndAdd(numTokens + 1));

		for (int i = 0; i < n.getNumChild(); ++i) {
			traverse(n.getChild(i), preCounter, postCounter);
		}

		postNumber.put(n, postCounter.addAndGet(numTokens + 1));
	}

	private void toTuples(String file) {
		int fileId = fidStore.getIdForFile(file);

		for (Map.Entry<ASTNode, Integer> kv : postNumber.entrySet()) {
			ASTNode n = kv.getKey();
			String relName = n.getClass().getSimpleName();

			int post = kv.getValue();
			int pre = preNumber.get(n);

			for (int i = 0; i < n.getNumChild(); ++i) {
				int childPost = postNumber.get(n.getChild(i));
				int childPre = preNumber.get(n.getChild(i));

				sink.getAST().insertTuple(relName, makeFullId(pre, post, fileId), i, makeFullId(childPre, childPost, fileId), "");
				sink.getSrcLoc().insertTuple(makeFullId(pre, post, fileId), n.startLine(), n.startColumn(), n.endLine(), n.endColumn(), file);
			}


			java.util.List<Pair<String, String>> tokens = namedTokens(n);
			for (int i = 0; i < tokens.size(); ++i) {
				int tokenPost = post - (i + 1);
				int tokenPre = pre + (i + 1);
				sink.getAST().insertTuple(relName, makeFullId(pre, post, fileId), i + n.getNumChild(), makeFullId(tokenPre, tokenPost, fileId), "");
				sink.getAST().insertTuple(tokens.get(i).getLeft(), makeFullId(tokenPre, tokenPost, fileId),  0, 0, tokens.get(i).getRight());
			}

			if (tokens.isEmpty() && n.getNumChild() == 0) {
				sink.getAST().insertTuple(relName, makeFullId(pre, post, fileId), -1, -1, "");
				sink.getSrcLoc().insertTuple(makeFullId(pre, post, fileId), n.startLine(), n.startColumn(), n.endLine(), n.endColumn(), file);
			}
		}
	}
}
