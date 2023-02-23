package clang;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;

import org.junit.jupiter.api.Test;

import eval.EvaluationContext;
import lang.CTestUtil;
import lang.c.obj.ast.ASTNode;
import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;
import se.lth.sep.ParseTree;

import beaver.Symbol;

public class ASTImporterTest {
	@Test
	public void test1() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/ast_importer/test1.c");
		root.prettyPrint(System.out);

		ASTTranslator translator = new ASTTranslator();
		ASTNode tRoot = translator.translate(root);
		tRoot.debugPrint(System.out);
	}

	@Test
	public void test2() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/ast_importer/for1.c");
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

		dp.project("tests/clang/ast_importer/for1.c");


		// for (TupleWrapper t : ((RelationWrapper) sink.getAST()).tuples()) {
		// 	System.out.println(t);
		// }
	}

	public static void internalParse(String file, PrintStream out) throws IOException {
		java.util.List<Symbol> tokens =  CTestUtil.scan(new lang.c.obj.ast.ObjLangScanner(new FileReader(file)));
		java.util.List<ParseTree> parseTrees = CTestUtil.parse(tokens, lang.c.obj.ast.ObjLangParserSEP.n_translation_unit,
															   lang.c.obj.ast.ASTBuilder.getInstance(),
															   lang.c.obj.ast.ObjLangParserSEP.Terminals.NAMES
															   );

		for (lang.c.obj.ast.ASTNode ast : CTestUtil.<lang.c.obj.ast.ASTNode>buildAST(tokens, parseTrees,
																					 lang.c.obj.ast.ASTBuilder.getInstance())) {
			ast.debugPrint(out);
			out.println("============================================================");
		}
	}


	@Test
	public void testCompareAST1() throws IOException {
		String src = "tests/clang/ast_importer/for1.c";
		AST.Node clangRoot = new ASTImporter().importAST(src);
		clangRoot.prettyPrint(new PrintStream(new File("for1.clang")));
		internalParse(src, new PrintStream(new File("for1.internal")));
	}
}
