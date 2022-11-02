package clang;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import eval.EvaluationContext;
import lang.ast.FormalPredicate;
import lang.c.obj.ast.ASTNode;
import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;
import lang.relation.RelationWrapper;
import lang.relation.RelationWrapper.TupleWrapper;

public class ASTImporterTest {
	@Test
	public void test1() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/test1.c");
		root.prettyPrint(System.out);

		ASTTranslator translator = new ASTTranslator();
		ASTNode tRoot = translator.translate(root);
		tRoot.debugPrint(System.out);
	}

	@Test
	public void test2() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/for1.c");
		root.prettyPrint(System.out);

		ASTTranslator translator = new ASTTranslator();
		ASTNode tRoot = translator.translate(root);
		tRoot.debugPrint(System.out);

	}

	private static DatalogProjectionSink makeSink() {
		return new DatalogProjectionSink(new EvaluationContext(),
										 null,
										 null,
										 null,
										 null);

	}

	@Test
	public void test3() throws IOException {
		DatalogProjectionSink sink = makeSink();
		DatalogProjection dp = new DatalogProjection(new FileIdDatabase(), sink);

		dp.project("tests/clang/for1.c");

		// for (TupleWrapper t : ((RelationWrapper) sink.getAST()).tuples()) {
		// 	System.out.println(t);
		// }
	}
}
