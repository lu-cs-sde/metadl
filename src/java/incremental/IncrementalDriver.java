package incremental;

import java.io.File;
import java.io.IOException;
import java.security.MessageDigest;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import eval.EvaluationContext;
import eval.Relation2;
import lang.ast.FormalPredicate;
import lang.ast.PredicateType;
import lang.ast.ProgramRepresentation;
import lang.io.CSVUtil;
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

	private static RelationWrapper makeRelation(EvaluationContext ctx, PredicateType t) {
		return new RelationWrapper(ctx, new Relation2(t.arity()), t);
	}

	public StandaloneDatalogProjectionSink() {
		EvaluationContext ctx = new EvaluationContext();
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
		writeToFileHelper(ast, root, ProgramRepresentation.AST.name());
		writeToFileHelper(provenance, root, ProgramRepresentation.ATTR_PROVENANCE.name());
		writeToFileHelper(attributes, root, ProgramRepresentation.ATTR.name());
		writeToFileHelper(srcLoc, root, ProgramRepresentation.SRC.name());
		writeToFileHelper(nta, root, ProgramRepresentation.NTA.name());
	}
}

public class IncrementalDriver {
	private File common;
	private File prog;
	private File srcs;
	private FileIdDatabase fileIdDb;
	private File fileIdDbFile;
	private File pathHashFile;

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

	public void generate(List<File> sourceFiles) throws IOException {
		// ensure that all directories exist
		common.mkdir();
		prog.mkdir();
		srcs.mkdir();

		// create a fresh file-id database
		fileIdDb = new FileIdDatabase();
		Map<String, String> fileToHash = new TreeMap<String, String>();

		for (File f : sourceFiles) {
			org.extendj.ast.Program p = createProgram(fileIdDb);
			p.addSourceFile(f.getPath());
			checkProgram(p);

			// generate the Datalog projection
			StandaloneDatalogProjectionSink sink = new StandaloneDatalogProjectionSink();
			DatalogProjection2 proj = new DatalogProjection2(p, sink, fileIdDb);
			proj.generate();

			String pathHash = hash(f);

			fileToHash.put(f.getPath(), pathHash);

			// now dump it to the right files
			File srcDir = new File(srcs, pathHash);
			srcDir.mkdir();
			sink.writeToFile(srcDir);
		}

		// store the file-id database
		fileIdDb.storeToFile(fileIdDbFile.getPath());
		// store the file name to source dir map
		CSVUtil.writeMap(fileToHash, pathHashFile.getPath());
	}

	public void update(List<File> modifiedFiles, List<File> removedFiles,
					   List<File> addedFiles) {
	}
}
