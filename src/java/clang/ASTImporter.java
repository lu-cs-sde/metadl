package clang;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.TypeAdapterFactory;
import com.google.gson.internal.Streams;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import lang.java.obj.DatalogProjectionSink;


public class ASTImporter {
	private final List<String> clangCmd = List.of("clang", "-fsyntax-only", "-Xclang", "-ast-dump=json");

	public void importAST(String file, DatalogProjectionSink s) throws IOException {
		List<String> actualCmd = new ArrayList<>(clangCmd);
		actualCmd.add(file);
		ProcessBuilder b = new ProcessBuilder(actualCmd);

		Process p = b.start();

		GsonBuilder builder = new GsonBuilder();

		ASTTypeAdapterFactory astTypeAdapter = new ASTTypeAdapterFactory();

		for (Class c : ClangAST.getASTNodeTypes()) {
			astTypeAdapter.registerNodeType(c);
		}

		builder.registerTypeAdapterFactory(astTypeAdapter);
		builder.setLenient();
		Gson gson = builder.create();
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
