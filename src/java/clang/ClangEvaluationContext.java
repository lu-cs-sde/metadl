package clang;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import clang.swig.ClangClog;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorLong;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;
import eval.EvaluationContext;
import eval.Tuple;
import lang.ast.ExternalLiteral;
import lang.cons.ObjLangASTNode;


public class ClangEvaluationContext extends EvaluationContext {
	private ClangClogBuilder builder;
	private ClangClog clog;

	public ClangEvaluationContext(Set<String> srcs) {
		System.loadLibrary("clangClogSWIG");

		VectorString files = new VectorString(srcs);

		builder = new ClangClogBuilder(files);
		clog = builder.build();
	}

	public List<Integer> registerExternalLiteral(ExternalLiteral l) {
		List<Integer> res = new ArrayList<>();

		List<ObjLangASTNode> ASTs = (List<ObjLangASTNode>) l.getExternalPayload();

		for (ObjLangASTNode Node : ASTs) {
			lang.c.pat.ast.ASTNode CNode = (lang.c.pat.ast.ASTNode) Node;
			Node.debugPrint(System.out);
			String matcher = CNode.matcher().genMatcher();

			int matcherId = clog.registerMatcher(matcher, true/* Global */);

			if (matcherId < 0) {
				System.err.println("Failed to register matcher " + matcher);
				throw new RuntimeException();
			}

			res.add(matcherId);
			System.out.println(matcher);
		}

		return res;
	}

	private boolean globalMatchersDone = false;

	public Stream<VectorLong> lookup(List<Integer> matcherIds) {
		if (!globalMatchersDone) {
			synchronized (this) {
				if (!globalMatchersDone) {
					clog.runGlobalMatchers();
					globalMatchersDone = true;
				}
			}
		}

		Stream<VectorLong> result = Stream.ofNullable(null);
		for (int matcherId : matcherIds) {
			VectorVectorLong matches = clog.matchFromRoot(matcherId);
			result = Stream.concat(result, matches.stream());
		}

		return result;
	}
}
