package clang;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;


public class ASTImporter {
	private final List<String> clangCmd = List.of("clang", "-fsyntax-only", "-Xclang", "-ast-dump=json");

	public AST.Node importAST(String file) throws IOException {
		List<String> actualCmd = new ArrayList<>(clangCmd);
		actualCmd.add(file);
		ProcessBuilder b = new ProcessBuilder(actualCmd);

		Process p = b.start();

		GsonBuilder builder = new GsonBuilder();

		ASTTypeAdapterFactory astTypeAdapter = new ASTTypeAdapterFactory();

		for (Class c : AST.getASTNodeTypes()) {
			astTypeAdapter.registerNodeType(c);
		}

		Pattern stmtName = Pattern.compile(".*Stmt");
		Pattern declName = Pattern.compile(".*Decl");
		Pattern exprName = Pattern.compile(".*(Expr|Operator)");

		astTypeAdapter.registerNodeKindFalback(name -> {
				if (stmtName.matcher(name).matches()) {
					return "Stmt";
				} else if (declName.matcher(name).matches()) {
					return "Decl";
				} else if (exprName.matcher(name).matches()) {
					return "Expr";
				} else {
					return null;
				}
			});

		builder.registerTypeAdapterFactory(astTypeAdapter);
		builder.setLenient();
		Gson gson = builder.create();
		AST.Node root = gson.fromJson(new InputStreamReader(p.getInputStream()), AST.Node.class);

		try {
			p.waitFor();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}


		root.patchLocations();

		return root;
	}
}
