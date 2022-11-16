package clang;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import lang.io.FileUtil.OutputConsumer;
import lang.io.SimpleLogger;


public class ASTImporter {
	private final List<String> clangCmdPrefix = List.of("clang", "-fsyntax-only");
	private final List<String> clangCmdSuffix = List.of("-Xclang", "-ast-dump=json");

	public AST.Node importAST(String file) throws IOException {
		return importAST(file, Collections.emptyList());
	}

	public AST.Node importAST(String file, List<String> clangArgs) throws IOException {
		List<String> actualCmd = new ArrayList<>(clangCmdPrefix);
		actualCmd.add(file);
		actualCmd.addAll(clangArgs);
		actualCmd.addAll(clangCmdSuffix);

		SimpleLogger.logger().log("Running clang: " + actualCmd);

		ProcessBuilder b = new ProcessBuilder(actualCmd);

		Process p = b.start();

		// read the error output
		new Thread(new OutputConsumer(p.getErrorStream())).start();

		// build a JSON reader
		GsonBuilder builder = new GsonBuilder();
		ASTTypeAdapterFactory astTypeAdapter = new ASTTypeAdapterFactory();

		for (Class c : AST.getASTNodeTypes()) {
			astTypeAdapter.registerNodeType(c);
		}

		Pattern stmtName = Pattern.compile(".*Stmt");
		Pattern declName = Pattern.compile(".*Decl");
		Pattern exprName = Pattern.compile(".*(Expr|Operator|Literal).*");

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
			int exitcode = p.waitFor();
			if (exitcode != 0) {
				SimpleLogger.logger().error("Clang command failed. " + b.command().toString());
			}
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}

		if (root != null)
			root.patchLocations();

		return root;
	}
}
