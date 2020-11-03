package lang.io;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.WildcardFileFilter;

import eval.EvaluationContext;
import eval.Relation2;

import org.apache.commons.io.filefilter.TrueFileFilter;
import org.apache.commons.io.FileUtils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import lang.CmdLineOpts;
import lang.ast.LangParser;
import lang.ast.LangScanner;
import lang.ast.Program;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;
import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;

public class FileUtil {
	public static String changeExtension(String filename, String newExtension) {
		int index = filename.lastIndexOf('.');
		if (index != -1) {
			return filename.substring(0, index) + newExtension;
		} else {
			return filename + newExtension;
		}
	}

    public static String fileNameNoExtension(String path) {
        String fn = fileName(path);
        int index = fn.lastIndexOf('.');
        if(index == -1) return fn;
        return fn.substring(0, index);
    }

	public static String fileName(String path) {
		Path p = Paths.get(path);
		return p.getFileName().toString();
	}

	public static String normalize(String dir) {
		if(dir.length() == 0) return dir;
		if(dir.charAt(dir.length() - 1) == '/') return dir.substring(0, dir.length() - 1);
		return dir;
	}

	public static Program parse(String program) throws IOException, beaver.Parser.Exception {
		LangScanner scanner = new LangScanner(new StringReader(program));
		LangParser parser = new LangParser();
		return (Program)parser.parse(scanner);
	}

	public static Program parse(File file) throws IOException, beaver.Parser.Exception {
		LangScanner scanner = new LangScanner(new FileReader(file));
		LangParser parser = new LangParser();
		return (Program)parser.parse(scanner);
	}


	public static CmdLineOpts parseDescription(String s) throws IOException, beaver.Parser.Exception {
		return lang.CmdLineOpts.parseCmdLineArgs(s.split("\\s+"));
	}

	public static List<File> flattenFilesAndDirs(List<File> files, String ext) {
		List<File> ret = new ArrayList<>();
		for (File fileOrDir : files) {
			if (fileOrDir.isDirectory()) {
				IOFileFilter ff = new WildcardFileFilter("*.java");
				Iterator<File> it = FileUtils.iterateFiles(fileOrDir, ff, TrueFileFilter.INSTANCE);
				while (it.hasNext()) {
					File f = it.next();
					ret.add(f);
				}
			} else {
				ret.add(fileOrDir);
			}
		}
		return ret;
	}

	public static void loadJavaSources(EvaluationContext ctx,
									   DatalogProjectionSink tupleSink,
									   java.util.List<String> locs) throws IOException {
		org.extendj.ast.Program p = new org.extendj.ast.Program();
		String fileIdDb = "analyzed-files.csv";

		p.trace().setReceiver(p.provenance);
		SimpleLogger.logger().info("Using an empty file-to-id database");
		p.fileIdStorage = new FileIdDatabase();


		// Set the path to the Java runtime
		String bootCP = System.getenv().get("METADL_JAVA_RT");
		if (bootCP != null)
			p.options().setValueForOption(bootCP, "-bootclasspath");
		String CP = System.getenv().get("METADL_JAVA_CP");
		if (CP != null)
			p.options().setValueForOption(CP, "-classpath");

		// Walk all the java files in the directory and add them to the program
		for (File src : flattenFilesAndDirs(locs.stream().map(f -> new File(f)).collect(Collectors.toList()), "*.java")) {
			p.addSourceFile(src.getPath());
		}

		// Some sanity check
		org.extendj.ast.TypeDecl object = p.lookupType("java.lang", "Object");
		if (object.isUnknown()) {
			// If we try to continue without java.lang.Object, we'll just get a stack overflow
			// in member lookups because the unknown (Object) type would be treated as circular.
			throw new RuntimeException("Error: java.lang.Object is missing."
									   + " The Java standard library was not found.");
		}

		// Generate the program relation
		lang.java.obj.DatalogProjection2 proj2 = new lang.java.obj.DatalogProjection2(p, tupleSink, p.fileIdStorage);
		proj2.generate();
	}
}
