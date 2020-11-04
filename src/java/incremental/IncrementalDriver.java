package incremental;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.security.MessageDigest;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

import eval.EvaluationContext;
import eval.Relation2;
import lang.CmdLineOpts;
import lang.ast.FormalPredicate;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.ProgramRepresentation;
import lang.ast.StandardPrettyPrinter;
import lang.io.CSVUtil;
import lang.Compiler;
import static lang.io.SimpleLogger.*;
import lang.java.obj.DatalogProjection2;
import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;

class StandaloneDatalogProjectionSink extends DatalogProjectionSink {
	@Override public DatalogProjectionSink remap(Map<FormalPredicate, TupleInserter> sinkRemap) {
		throw new RuntimeException("Remapping is not supported.");
	}

	private RelationWrapper ast;
	private RelationWrapper provenance;
	private RelationWrapper attributes;
	private RelationWrapper srcLoc;
	private RelationWrapper nta;
	private String relPrefix;

	private static RelationWrapper makeRelation(EvaluationContext ctx, PredicateType t) {
		return new RelationWrapper(ctx, new Relation2(t.arity()), t);
	}

	public StandaloneDatalogProjectionSink(String relPrefix) {
		EvaluationContext ctx = new EvaluationContext();
		this.relPrefix = relPrefix;
		ast = makeRelation(ctx, FormalPredicate.programRepresentationType(ProgramRepresentation.AST));
		provenance = makeRelation(ctx, FormalPredicate.programRepresentationType(ProgramRepresentation.ATTR_PROVENANCE));
		attributes = makeRelation(ctx, FormalPredicate.programRepresentationType(ProgramRepresentation.ATTR));
		srcLoc = makeRelation(ctx, FormalPredicate.programRepresentationType(ProgramRepresentation.SRC));
		nta = makeRelation(ctx, FormalPredicate.programRepresentationType(ProgramRepresentation.NTA));
	}

	@Override public RelationWrapper getAST() {
		return ast;
	}

	@Override public RelationWrapper getProvenance() {
		return provenance;
	}

	@Override public RelationWrapper getAttributes() {
		return attributes;
	}

	@Override public RelationWrapper getSrcLoc() {
		return srcLoc;
	}

	@Override public RelationWrapper getNTA() {
		return nta;
	}

	private void writeToFileHelper(RelationWrapper rel, File root, String name) throws IOException {
		CSVUtil.writeRelation(rel.getContext(), rel.type(), rel.getRelation(), new File(root, name).getPath());
	}

	public void writeToFile(File root) throws IOException {
		writeToFileHelper(ast, root, relPrefix + ProgramRepresentation.AST.getPredicateName() + ".csv");
		writeToFileHelper(provenance, root, relPrefix + ProgramRepresentation.ATTR_PROVENANCE.getPredicateName() + ".csv");
		writeToFileHelper(attributes, root, relPrefix + ProgramRepresentation.ATTR.getPredicateName() + ".csv");
		writeToFileHelper(srcLoc, root, relPrefix + ProgramRepresentation.SRC.getPredicateName() + ".csv");
		writeToFileHelper(nta, root, relPrefix + ProgramRepresentation.NTA.getPredicateName() + ".csv");
	}
}

public class IncrementalDriver {
	private File common;
	private File prog;
	private File srcs;
	private FileIdDatabase fileIdDb;
	private File fileIdDbFile;
	private File pathHashFile;

	private Map<String, String> fileToHash;

	public IncrementalDriver(File root) {
		// folders
		this.common = new File(root, "common");
		this.prog = new File(root, "prog");
		this.srcs = new File(root, "srcs");
		// files
		this.fileIdDbFile = new File(common, "fileid.csv");
		// fileHash
		this.pathHashFile = new File(common, "filehash.csv");
	}

	public void init() throws IOException {
		// ensure that all directories exist
		common.mkdir();
		prog.mkdir();
		srcs.mkdir();

		if (fileIdDbFile.exists()) {
			logger().info("Loading the file ID database from " + fileIdDbFile);
			fileIdDb = FileIdDatabase.loadFromFile(fileIdDbFile.getPath());
		} else {
			// create a fresh file-id database
			fileIdDb = new FileIdDatabase();
		}

		fileToHash = new TreeMap<String, String>();
		if (pathHashFile.exists()) {
			logger().info("Loading path hashes from " + pathHashFile);
			CSVUtil.readMap(fileToHash, Function.identity(), Function.identity(), pathHashFile.getPath());
		}
	}

	public void shutdown() throws IOException {
		// store the file-id database
		fileIdDb.storeToFile(fileIdDbFile.getPath());
		// store the file name to source dir map
		CSVUtil.writeMap(fileToHash, pathHashFile.getPath());
	}

	private org.extendj.ast.Program createProgram(org.extendj.ast.FileIdStorage fs) {
		org.extendj.ast.Program p = new org.extendj.ast.Program();
		// log trace events to the provenance stack machine
		p.trace().setReceiver(p.provenance);
		// set the node id source
		p.fileIdStorage = fs;

		// Set the path to the Java runtime
		String bootCP = System.getenv().get("METADL_JAVA_RT");
		if (bootCP != null)
			p.options().setValueForOption(bootCP, "-bootclasspath");
		String CP = System.getenv().get("METADL_JAVA_CP");
		if (CP != null)
			p.options().setValueForOption(CP, "-classpath");

		return p;
	}

	protected static String hash(File f) {
		try {
			MessageDigest md = MessageDigest.getInstance("SHA-1");
			byte[] md5 = md.digest(f.getPath().getBytes("UTF-8"));
			StringBuffer sb = new StringBuffer();
			for (byte b : md5) {
				sb.append(String.format("%02x", b));
			}
			return sb.toString();
		}
		catch (Exception e) {}
		return null;
	}

	private void checkProgram(org.extendj.ast.Program p) {
		// Some sanity check
		org.extendj.ast.TypeDecl object = p.lookupType("java.lang", "Object");
		if (object.isUnknown()) {
			// If we try to continue without java.lang.Object, we'll just get a stack overflow
			// in member lookups because the unknown (Object) type would be treated as circular.
			throw new RuntimeException("Error: java.lang.Object is missing."
									   + " The Java standard library was not found.");
		}
	}

	public void generate(List<File> sourceFiles, String relPrefix) throws IOException {
		for (File f : sourceFiles) {
			org.extendj.ast.Program p = createProgram(fileIdDb);
			p.addSourceFile(f.getPath());
			checkProgram(p);

			// generate the Datalog projection
			StandaloneDatalogProjectionSink sink = new StandaloneDatalogProjectionSink(relPrefix);
			DatalogProjection2 proj = new DatalogProjection2(p, sink, fileIdDb);
			proj.generate();

			String pathHash = hash(f);

			fileToHash.put(f.getPath(), pathHash);

			// now dump it to the right files
			File srcDir = new File(srcs, pathHash);
			srcDir.mkdir();
			sink.writeToFile(srcDir);
		}
	}

	public void runLocalProgram(Program local) throws IOException {
		StandardPrettyPrinter<Program> lpp =
			new StandardPrettyPrinter<>(new PrintStream(new File(prog, "local.mdl")));
		lpp.prettyPrint(local);

		for (String h : fileToHash.values()) {
			File factDir = new File(srcs, h);
			File outDir = factDir;

			CmdLineOpts opts = new CmdLineOpts();
			opts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
			opts.setFactsDir(factDir.getPath());
			opts.setOutputDir(outDir.getPath());

			Compiler.checkProgram(local, opts);
			local.eval(opts);
			local.clearRelations();
		}
	}

	public void concatenateLocalResults() {

	}

	public void update(List<File> modifiedFiles, List<File> removedFiles,
					   List<File> addedFiles) {
	}
}
