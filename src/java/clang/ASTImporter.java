package clang;

import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.io.InputStreamReader;
import lang.java.obj.DatalogProjectionSink;
import com.google.gson.JsonParser;
import com.google.gson.JsonElement;
import com.google.gson.Gson;


public class ASTImporter {
	private final List<String> clangCmd = List.of("clang", "-fsyntax-only", "-Xclang", "-ast-dump=json");

	public void importAST(String file, DatalogProjectionSink s) throws IOException {
		List<String> actualCmd = new ArrayList<>(clangCmd);
		actualCmd.add(file);
		ProcessBuilder b = new ProcessBuilder(actualCmd);

		Process p = b.start();

		Gson gson = new Gson();
		ClangAST.Node root = gson.fromJson(new InputStreamReader(p.getInputStream()), ClangAST.Node.class);

		root.patchLocations();
		root.prettyPrint(System.out);

		try {
			p.waitFor();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
	}
}
