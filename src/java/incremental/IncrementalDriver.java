package incremental;

import java.sql.Statement;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.security.MessageDigest;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;

import eval.EvaluationContext;
import eval.Relation2;
import lang.CmdLineOpts;
import lang.ast.FormalPredicate;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.ProgramRepresentation;
import lang.ast.StandardPrettyPrinter;
import lang.io.CSVUtil;
import lang.io.SQLUtil;
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

	private EnumMap<ProgramRepresentation, RelationWrapper> relations = new EnumMap<>(ProgramRepresentation.class);

	private String relPrefix;

	private static RelationWrapper makeRelation(EvaluationContext ctx, PredicateType t) {
		return new RelationWrapper(ctx, new Relation2(t.arity()), t);
	}

	public StandaloneDatalogProjectionSink(String relPrefix) {
		EvaluationContext ctx = new EvaluationContext();
		this.relPrefix = relPrefix;
		for (ProgramRepresentation pr : ProgramRepresentation.values()) {
			relations.put(pr, makeRelation(ctx, FormalPredicate.programRepresentationType(pr)));
		}
	}

	@Override public RelationWrapper getAST() {
		return relations.get(ProgramRepresentation.AST);
	}

	@Override public RelationWrapper getProvenance() {
		return relations.get(ProgramRepresentation.ATTR_PROVENANCE);
	}

	@Override public RelationWrapper getAttributes() {
		return relations.get(ProgramRepresentation.ATTR);
	}

	@Override public RelationWrapper getSrcLoc() {
		return relations.get(ProgramRepresentation.SRC);
	}

	@Override public RelationWrapper getNTA() {
		return relations.get(ProgramRepresentation.NTA);
	}

	private void writeToFileHelper(RelationWrapper rel, File root, String name) throws IOException {
		CSVUtil.writeRelation(rel.getContext(), rel.type(), rel.getRelation(), new File(root, name + ".csv").getPath());
	}

	public void writeToFile(File root) throws IOException {
		for (ProgramRepresentation pr : ProgramRepresentation.values()) {
			writeToFileHelper(relations.get(pr), root, relPrefix + pr.getPredicateName());
		}
	}

	private void writeToDBHelper(RelationWrapper rel, File dbFile, String table) throws SQLException {
		SQLUtil.writeRelation(rel.getContext(), rel.type(), rel.getRelation(), dbFile.getPath(), table);
	}

	public void writeToDB(File dbFile) throws SQLException {
		for (ProgramRepresentation pr : ProgramRepresentation.values()) {
			writeToDBHelper(relations.get(pr), dbFile, relPrefix + pr.getPredicateName());
		}
	}
}

public class IncrementalDriver {
	private File common;
	private File prog;
	private File srcs;
	private FileIdDatabase fileIdDb;
	private File fileIdDbFile;
	private File pathHashFile;
	private File progDbFile;

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
		// program database
		this.progDbFile = new File(srcs, "program.db");
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

	public void generate(List<File> sourceFiles, String relPrefix) throws IOException, SQLException {
		for (File f : sourceFiles) {
			logger().debug("Extracting AST facts from source " + f.getPath() + ".");
			org.extendj.ast.Program p = createProgram(fileIdDb);
			p.addSourceFile(f.getPath());
			checkProgram(p);

			String pathHash = hash(f);

			// generate the Datalog projection
			StandaloneDatalogProjectionSink sink = new StandaloneDatalogProjectionSink("cu_" + pathHash + "$" + relPrefix);
			DatalogProjection2 proj = new DatalogProjection2(p, sink, fileIdDb);
			proj.generate();

			fileToHash.put(f.getPath(), pathHash);

			sink.writeToDB(progDbFile);
		}
	}

	public void runLocalProgram(Program local) throws IOException, SQLException {
		StandardPrettyPrinter<Program> lpp =
			new StandardPrettyPrinter<>(new PrintStream(new File(prog, "local.mdl")));
		lpp.prettyPrint(local);

		// parallelize this loop
		for (String h : fileToHash.values()) {
			logger().debug("Running the local program on " + h + ".");
			CmdLineOpts opts = new CmdLineOpts();
			opts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
			opts.setSqlDbFile(progDbFile.getPath());
			// TODO: this prefix is hacky; use temporary views to the
			// right relation instead
			opts.setRelationNamePrefix("cu_" + h + "$");

			Compiler.checkProgram(local, opts);
			local.eval(opts);
			local.clearRelations();
		}
	}

	private void createGlobalView(Connection conn, String localOutput) throws SQLException {
		Statement stmt = conn.createStatement();
		stmt.executeUpdate("DROP VIEW IF EXISTS " + localOutput);

		String unionView = "CREATE VIEW " + localOutput + " AS ";
		unionView += String.join(" UNION ",
								 fileToHash.values().stream()
								 .map(h -> "SELECT * FROM cu_" + h + "$" + localOutput)
								 .collect(Collectors.toUnmodifiableList()));

		logger().debug("Creating a global view, " + unionView);
		stmt.executeUpdate(unionView);
	}

	private void createGlobalViews(Set<String> localOutputs) throws SQLException {
		Connection conn = DriverManager.getConnection("jdbc:sqlite:" + progDbFile.getPath());
		conn.setAutoCommit(false);
		for (String l : localOutputs) {
			createGlobalView(conn, l);
		}
		conn.commit();
		conn.close();
	}

	public void runGlobalProgram(Set<String> localOutputs, Program global) throws SQLException, IOException {
		StandardPrettyPrinter<Program> pp =
			new StandardPrettyPrinter<>(new PrintStream(new File(prog, "global.mdl")));
		pp.prettyPrint(global);

		createGlobalViews(localOutputs);
		logger().debug("Running the global program");
		CmdLineOpts opts = new CmdLineOpts();
		opts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
		opts.setSqlDbFile(progDbFile.getPath());
		Compiler.checkProgram(global, opts);
		global.eval(opts);
	}

	public void update(List<File> modifiedFiles, List<File> removedFiles,
					   List<File> addedFiles) {
	}
}
