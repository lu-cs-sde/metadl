package clang;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import lang.c.obj.ast.ASTNode;

public class ASTImporterTest {
	@Test
	public void test1() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/test1.c");
		root.prettyPrint(System.out);

		ClangASTTranslator translator = new ClangASTTranslator();
		ASTNode tRoot = translator.translate(root);
		tRoot.debugPrint(System.out);
	}

	@Test
	public void test2() throws IOException {
		AST.Node root = new ASTImporter().importAST("tests/clang/for1.c");
		root.prettyPrint(System.out);

		ClangASTTranslator translator = new ClangASTTranslator();
		ASTNode tRoot = translator.translate(root);
		tRoot.debugPrint(System.out);

	}
}
