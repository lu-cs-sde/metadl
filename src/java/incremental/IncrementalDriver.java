package incremental;

import java.sql.Statement;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.CopyOption;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;
import org.extendj.ast.ClassSource;
import org.extendj.ast.CompilationUnit;

import eval.EvaluationContext;
import eval.Relation2;
import lang.CmdLineOpts;
import lang.ast.AnalyzeBlock;
import lang.ast.FormalPredicate;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.ProgramRepresentation;
import lang.ast.StandardPrettyPrinter;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SQLUtil;
import lang.Compiler;
import static lang.io.SimpleLogger.*;
import static prof.Profile.*;
import lang.java.obj.DatalogProjection2;
import lang.java.obj.DatalogProjectionSink;
import lang.java.obj.FileIdDatabase;
import lang.relation.RelationWrapper;
import lang.relation.TupleInserter;
import lang.relation.RelationWrapper.TupleWrapper;
import swig.SWIGSouffleProgram;
import swig.SWIGUtil;

class HybridDatalogProjectionSink extends DatalogProjectionSink {
	@Override public DatalogProjectionSink remap(Map<FormalPredicate, TupleInserter> sinkRemap) {
		throw new RuntimeException("Remapping is not supported.");
	}

	private EnumMap<ProgramRepresentation, TupleInserter> relations = new EnumMap<>(ProgramRepresentation.class);

	public HybridDatalogProjectionSink() {
	}

	public void setTupleInserter(ProgramRepresentation pr, TupleInserter ti) {
		relations.put(pr, ti);
	}

	@Override public TupleInserter getAST() {
		return relations.getOrDefault(ProgramRepresentation.AST, TupleInserter.NULL);
	}

	@Override public TupleInserter getProvenance() {
		return relations.getOrDefault(ProgramRepresentation.ATTR_PROVENANCE, TupleInserter.NULL);
	}

	@Override public TupleInserter getAttributes() {
		return relations.getOrDefault(ProgramRepresentation.ATTR, TupleInserter.NULL);
	}

	@Override public TupleInserter getSrcLoc() {
		return relations.getOrDefault(ProgramRepresentation.SRC, TupleInserter.NULL);
	}

	@Override public TupleInserter getNTA() {
		return relations.getOrDefault(ProgramRepresentation.NTA, TupleInserter.NULL);
	}
}

public class IncrementalDriver {
	private File common;
	private File prog;
	private File srcs;

	private Connection progDbConnection;
	private File progDbFile;
	private File updateSouffleProg;
	private File fusedSouffleLib;
	private File externalClassesFile;

	private ProgramSplit progSplit;

	private boolean useSouffle;

	public IncrementalDriver(File root, ProgramSplit progSplit, boolean useSouffle) {
		this.useSouffle = useSouffle;
		this.progSplit = progSplit;
		// folders
		this.common = new File(root, "common");
		this.prog = new File(root, "prog");
		this.srcs = new File(root, "srcs");
		// program database
		this.progDbFile = new File(srcs, "program.db");
		// external classes
		this.externalClassesFile = new File(common, "extclass.csv");
		// souffle programs
		this.updateSouffleProg = new File(prog, "update");
		this.fusedSouffleLib = new File(prog, "libfused.so");
	}

	private FileIdDatabase fileIdDb;
	private Map<String, String> externalClasses = new TreeMap<>();

	/**
	   Creates the directory structure of the cache and ensures that
	   the local, global and update programs exist.
	 * @throws SQLException
	 */
	public void init() throws IOException, SQLException {
		// ensure that all directories exist
		common.mkdir();
		prog.mkdir();
		srcs.mkdir();

		progDbConnection = SQLUtil.connect(progDbFile.getPath());

		// load the file id database
		if (!SQLUtil.containsTable(progDbConnection, ProgramSplit.FILE_ID)) {
			logger().info("Using a fresh file ID database");
			fileIdDb = new FileIdDatabase();
		} else {
			fileIdDb = FileIdDatabase.loadFromTable(progDbConnection, ProgramSplit.FILE_ID);
		}

		// load the external classes
		if (externalClassesFile.exists()) {
			logger().info("Loading external classes database from " + externalClassesFile);
			CSVUtil.readMap(externalClasses, Function.identity(), Function.identity(), externalClassesFile.getPath());
		}

		// dumpProgram(progSplit.getLocalProgram(), new File(prog, "local.mdl"));
		// dumpProgram(progSplit.getGlobalProgram(), new File(prog, "global.mdl"));
		dumpProgram(progSplit.getUpdateProgram(), new File(prog, "update.mdl"));
		dumpProgram(progSplit.getFusedProgram(), new File(prog, "fused.mdl"));

		if (useSouffle) {
			if (!fusedSouffleLib.exists()) {
				File tmp = new File(prog, "fused.dl");
				Compiler.prettyPrintSouffle(progSplit.getFusedProgram(), tmp.getPath());
				compileSouffleLib(tmp, fusedSouffleLib);
			}

			if (!updateSouffleProg.exists()) {
				generateSouffleProgram(progSplit.getUpdateProgram(), updateSouffleProg, "-j 4 -p update.profile");
			} else {
				logger().info("Using existing Souffle update program from " + updateSouffleProg);
			}

		}

		// if (useSouffle) {
		// 	// generate the Souffle executables
		// 	if (!localSouffleProg.exists()) {
		// 		generateSouffleProgram(progSplit.getLocalProgram(), localSouffleProg, "-j 4 -p local.profile");
		// 	} else {
		// 		logger().info("Using existing Souffle local program from " + localSouffleProg);
		// 	}

		// 	if (!globalSouffleProg.exists()) {
		// 		generateSouffleProgram(progSplit.getGlobalProgram(), globalSouffleProg, "-j 4 -p global.profile");
		// 	} else {
		// 		logger().info("Using existing Souffle global program from " + globalSouffleProg);
		// 	}

		// }
	}

	public void shutdown() throws IOException, SQLException {
		// store the file-id database
		fileIdDb.storeToTable(progDbConnection, ProgramSplit.FILE_ID);
		// store the external classes file
		CSVUtil.writeMap(externalClasses, externalClassesFile.getPath());
		// the connection to the program db is closed only once for each
		// incremental run
		progDbConnection.close();
	}

	private static void compileSouffleProgram(File src, File exec, String extraArgs) throws IOException {
		// The -j 4 argument ensures that Souffle uses OpenMP in the generated code.
		// The number of threads is selected dynamically and the value 4 is not relevant.
		String cmd = "souffle " + extraArgs + " -w " + src.getPath() + " -o " + exec.getPath();
		logger().debug("Compiling Souffle program with: " + cmd);
		FileUtil.run(cmd);
	}

	private static void generateSouffleProgram(Program p, File path, String extraArgs) throws IOException {
		File tmp = new File(path.getPath() + ".dl");
		Compiler.prettyPrintSouffle(p, tmp.getPath());
		compileSouffleProgram(tmp, path, extraArgs);
	}

	private void compileSouffleLib(File src, File exec) throws IOException {
		String cmd = "souffle -j 4 -s java -w -p " + FileUtil.fileNameNoExtension(src.getPath()) + ".prof " + src.getPath();
		logger().debug("Compiling Souffle library with: " + cmd);
		FileUtil.run(cmd);
		Files.move(Paths.get("libSwigInterface.so"), exec.toPath(), StandardCopyOption.REPLACE_EXISTING);
	}

	private static void dumpProgram(Program p, File path) throws IOException {
		StandardPrettyPrinter<Program> lpp =
			new StandardPrettyPrinter<>(new PrintStream(path));
		lpp.prettyPrint(p);
	}

	private static org.extendj.ast.Program createProgram(org.extendj.ast.FileIdStorage fs) {
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

	/**
	   Run a sanity check on the program
	 */
	private static void checkProgram(org.extendj.ast.Program p) {
		// Some sanity check
		org.extendj.ast.TypeDecl object = p.lookupType("java.lang", "Object");
		if (object.isUnknown()) {
			// If we try to continue without java.lang.Object, we'll just get a stack overflow
			// in member lookups because the unknown (Object) type would be treated as circular.
			throw new RuntimeException("Error: java.lang.Object is missing."
									   + " The Java standard library was not found.");
		}
	}


	private Collection<ClassSource> generateFromSrc(File f, DatalogProjectionSink sink) throws IOException, SQLException {
		List<ClassSource> externalClasses = new ArrayList<>();

		profile().startTimer("object_file_compile", f.getPath());
		logger().debug("Extracting AST facts from source " + f.getPath() + ".");
		// TODO: is the Program object needed, or is the CU enough?
		org.extendj.ast.Program p = createProgram(fileIdDb);
		org.extendj.ast.CompilationUnit cu = p.addSourceFile(f.getPath());
		checkProgram(p);

		profile().stopTimer("object_file_compile", f.getPath());

		int fileTag = fileIdDb.getIdForFile(f.getPath());

		profile().startTimer("object_file_generate_relations", f.getPath());

		// generate the Datalog projection
		DatalogProjection2 proj = new DatalogProjection2(fileIdDb, p.provenance);

		Set<CompilationUnit> externalCUs = proj.generate(cu, sink);
		externalCUs.stream().filter(u -> !u.fromSource())
			.map(CompilationUnit::getClassSource).forEachOrdered(externalClasses::add);

		profile().stopTimer("object_file_generate_relations", f.getPath());

		return externalClasses;
	}

	private Collection<ClassSource> generateFromLib(ClassSource src, DatalogProjectionSink sink) throws IOException, SQLException {
		org.extendj.ast.Program externalClassProgram = createProgram(fileIdDb);

		// this class file is not in the database
		logger().debug("Adding class file to database " + src.relativeName() + ".");
		src.openInputStream();

		profile().startTimer("object_file_compile", src.relativeName());
		CompilationUnit externalCU = src.parseCompilationUnit(externalClassProgram);
		profile().stopTimer("object_file_compile", src.relativeName());
		int fileTag = fileIdDb.getIdForFile(src.relativeName());

		profile().startTimer("object_file_generate_relations", src.relativeName());
		DatalogProjection2 proj = new DatalogProjection2(fileIdDb, externalClassProgram.provenance);
		proj.generate(externalCU, sink);
		profile().stopTimer("object_file_generate_relations", src.relativeName());

		return Collections.emptyList();
	}

	/**
	   Generate the program representation for the given source files, and store it
	   to predicates with a given prefix
	 */
	private void generate(List<File> sourceFiles, DatalogProjectionSink sink) throws IOException, SQLException {
		StopWatch timer = StopWatch.createStarted();

		Set<ClassSource> externalClasses = new TreeSet<>(new Comparator<ClassSource>() {
			@Override
			public int compare(ClassSource arg0, ClassSource arg1) {
				return arg0.relativeName().compareTo(arg1.relativeName());
			}
			});

		// sourceFiles.parallelStream().filter(File::exists).map(f -> {
		// 													  try { return generateFromSrc(f, relPrefix); }
		// 													  catch (Exception e) { throw new RuntimeException(e);}
		// 													  finally { return Collections.<ClassSource>emptyList(); }})
		// 	.forEach(externalClasses::addAll);

		for (File f : sourceFiles) {
			if (!f.exists()) {
				logger().info("Skipping file " + f + ". File does not exist. ");
				continue;
			}

			externalClasses.addAll(generateFromSrc(f, sink));
		}


		for (ClassSource src : externalClasses) {
			if (this.externalClasses.put(src.relativeName(), src.relativeName()) != null) {
				// check whether the external class was already analyzed
				logger().debug("Skipping external class " + src.relativeName());
				continue;
			}

			generateFromLib(src, sink);
		}

		timer.stop();
		logger().time("Fact extraction from " + sourceFiles.size() + " files: " + timer.getTime() + " ms");
	}

	private static void deleteEntries(Connection conn, String table, int tagIndex, int fileId) throws SQLException {
		Statement stmt = conn.createStatement();
		stmt.executeUpdate("DELETE FROM '" + table + "' WHERE '" + table + "'.'" + tagIndex + "' = " + fileId);
	}

	private void delete(List<File> sourceFiles, String relPrefix) throws SQLException {
		for (File f : sourceFiles) {
			int fileId = fileIdDb.getIdForFile(f.getPath());

			for (String localTable  : progSplit.getCachedPredicates()) {
				FormalPredicate p = progSplit.getProgram().formalPredicateMap().get(localTable);
				// cached predicates are extended to the right with a tag field
				int tagIndex = p.realArity() + 1;
				deleteEntries(progDbConnection, localTable, tagIndex, fileId);
			}
		}

		progDbConnection.commit();
	}

	private RelationWrapper computeAnalyzedSources(String factDir) throws IOException, SQLException {
		Program prog = progSplit.getProgram();
		CmdLineOpts opts = new CmdLineOpts();
		opts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
		opts.setFactsDir(factDir);

		// evaluate the EDB and IMPORT predicate, to ensure
		// that the inputs to the analyze blocks can be evaluated in turn
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), opts);

		// find out which predicate defines the analyzed sources
		FormalPredicate srcPred = progSplit.getSourceRelation();
		srcPred.eval(prog.evalCtx());

		// copy the tuples from one relation to another
		RelationWrapper srcWrapper = new RelationWrapper(prog.evalCtx(), srcPred.relation2(), srcPred.type());

		return srcWrapper;
	}

	private static void setRelation(Program prog, String relName, RelationWrapper rel) {
		FormalPredicate pred = prog.formalPredicateMap().get(relName);
		pred.relation2().clear();
		RelationWrapper dst = new RelationWrapper(prog.evalCtx(), pred.relation2(), pred.type());
		dst.insertTuples(rel.tuples());
	}

	private static RelationWrapper getRelation(Program prog, String relName) {
		FormalPredicate pred = prog.formalPredicateMap().get(relName);
		RelationWrapper src = new RelationWrapper(prog.evalCtx(), pred.relation2(), pred.type());
		return src;
	}


	/**
	   Update the program representation for the given files.
	 */
	public void update(CmdLineOpts opts) throws IOException, SQLException {
		RelationWrapper analyzedSrcs = computeAnalyzedSources(opts.getFactsDir());

		List<File> visitFiles;
		List<File> deletedFiles;
		if (opts.getAction() == CmdLineOpts.Action.INCREMENTAL_INIT ||
			opts.getAction() == CmdLineOpts.Action.INCREMENTAL_INIT_INTERNAL) {
			visitFiles = analyzedSrcs.tuples().stream().map(t -> new File(t.getAsString(0))).collect(Collectors.toList());
			deletedFiles = Collections.emptyList();
			logger().debug("Initial incremental run with files " + visitFiles + ".");

			profile().setCounter("file_delta", "n_D", 0);
			profile().setCounter("file_delta", "n_A", visitFiles.size());
			profile().setCounter("file_delta", "n_M", 0);
			profile().setCounter("file_delta", "n_analyzed", visitFiles.size());
		} else {
			assert opts.getAction() == CmdLineOpts.Action.INCREMENTAL_UPDATE ||
				opts.getAction() == CmdLineOpts.Action.INCREMENTAL_UPDATE_INTERNAL;
			RelationWrapper visitFilesRel;
			RelationWrapper removeFilesRel;

			profile().startTimer("update_program", "total");

			if (useSouffle) {
				SQLUtil.clearRelation(progDbConnection, ProgramSplit.ANALYZED_SOURCES_RELATION);
				SQLUtil.writeRelation(progDbConnection, ProgramSplit.ANALYZED_SOURCES_RELATION, analyzedSrcs);

				// Run the update program
				String cmd = updateSouffleProg + " -D " + opts.getOutputDir() + " -F " + opts.getFactsDir()
					+ " -d " + progDbFile;
				logger().debug("Souffle command: " + cmd);

				// Souffle opens its own connection to the database, so we'll close the current one
				progDbConnection.close();
				FileUtil.run(cmd);
				progDbConnection = SQLUtil.connect(progDbFile.getPath());

				// Now read back the results
				visitFilesRel = SQLUtil.readRelation(progDbConnection, ProgramSplit.AST_VISIT_RELATION,
													 ProgramSplit.getTypeForUpdateRelation(ProgramSplit.AST_VISIT_RELATION));
				removeFilesRel = SQLUtil.readRelation(progDbConnection, ProgramSplit.AST_REMOVE_RELATION,
													  ProgramSplit.getTypeForUpdateRelation(ProgramSplit.AST_REMOVE_RELATION));
			} else {
				Program update = progSplit.getUpdateProgram();

				setRelation(update, ProgramSplit.ANALYZED_SOURCES_RELATION, analyzedSrcs);

				CmdLineOpts updateOpts = new CmdLineOpts();
				updateOpts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
				updateOpts.setSqlDbConnection(progDbConnection);
				updateOpts.setOutputDir(".");

				// evaluate the update program
				update.eval(updateOpts);

				visitFilesRel = getRelation(update, ProgramSplit.AST_VISIT_RELATION);
				removeFilesRel = getRelation(update, ProgramSplit.AST_REMOVE_RELATION);
			}

			profile().stopTimer("update_program", "total");

			deletedFiles = removeFilesRel.tuples().stream().map(t -> new File(t.getAsString(0))).collect(Collectors.toList());
			visitFiles = visitFilesRel.tuples().stream().map(t -> new File(t.getAsString(0))).collect(Collectors.toList());

			profile().setCounter("file_delta", "n_D",
								 analyzedSrcs.tuples().stream().filter(t -> t.getAsString(1).equals("D")).count());
			profile().setCounter("file_delta", "n_A",
								 analyzedSrcs.tuples().stream().filter(t -> t.getAsString(1).equals("A")).count());
			profile().setCounter("file_delta", "n_M",
								 analyzedSrcs.tuples().stream().filter(t -> t.getAsString(1).equals("M")).count());
			profile().setCounter("file_delta", "n_analyzed", visitFilesRel.tuples().size());

			logger().debug("Update incremental run with modified/added files " +
						   visitFiles + ", removed files " + deletedFiles + ".");
		}

		Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> swigProg =
			SWIGUtil.loadSWIGProgram(progSplit.getFusedProgram(), fusedSouffleLib.getAbsoluteFile(), "fused");

		Map<String, TupleInserter> tupleInsertersByName = swigProg.getRight().entrySet().stream()
			.collect(Collectors.toMap(e -> e.getKey().getPRED_ID(), e -> e.getValue()));

		// populate the program representation relations for each analyze block
		for (AnalyzeBlock b : progSplit.getProgram().analyzeBlocks()) {
			if (opts.getAction() == CmdLineOpts.Action.INCREMENTAL_UPDATE) {
				// remove the information for the deleted files, but also for the files
				// that need revisiting
				delete(Stream.concat(deletedFiles.stream(), visitFiles.stream()).collect(Collectors.toList()),
					   b.getContext().scopePrefix);
			}


			HybridDatalogProjectionSink sink = new HybridDatalogProjectionSink();
			// go over the program representation relations in this analyze block and figure
			// out whether they are used in the fused progam, if they are, then set the proper
			// inserters in the sink
			for (ProgramRepresentation pr : ProgramRepresentation.values()) {
				String predName = b.getScopePrefix() + pr.getPredicateName();
				TupleInserter ti = tupleInsertersByName.get(predName);
				if (ti != null) {
					logger().debug("Writing directly to the souffle relation " + predName);
					sink.setTupleInserter(pr, ti);
				}
			}

			generate(visitFiles, sink);
		}

		// now run the hybrid program
		{
			// Program fusedProgram = progSplit.getFusedProgram();
			// fusedProgram.evalEDB(fusedProgram.evalCtx(), opts);
			// fusedProgram.evalIMPORT(fusedProgram.evalCtx(), opts);

			// TODO: set the path to the database
			SWIGUtil.runSWIGProgram(swigProg.getLeft(), opts);
		}


		// runLocalProgram(Stream.concat(visitFiles.stream(), externalFiles.stream()).collect(Collectors.toList()), opts);
	}

	// private void runLocalProgram(List<File> visitFiles, CmdLineOpts opts) throws IOException, SQLException {
	// 	profile().startTimer("local_program", "total");

	// 	StopWatch timer = StopWatch.createStarted();
	// 	for (File file : visitFiles) {
	// 		runLocalProgram(opts, file);
	// 	}

	// 	profile().stopTimer("local_program", "total");
	// 	timer.stop();
	// 	logger().time("Running the local program on " + visitFiles.size() + " files:" + timer.getTime() + " ms");
	// }

	// /**
	//    Run the local program on all the files in the file database
	//  */
	// private void runLocalProgram(CmdLineOpts opts, File file) throws IOException, SQLException {
	// 	logger().debug("Running the local program on " + file + ".");

	// 	int fileId = fileIdDb.getIdForFile(file.getPath());

	// 	StopWatch timer = StopWatch.createStarted();
	// 	profile().startTimer("local_program", file.getPath());
	// 	if (useSouffle) {
	// 		String cmd = localSouffleProg.getPath() + " -D " + opts.getOutputDir() + " -F " + opts.getFactsDir() +
	// 			" -d " + progDbFile + " -t " + fileId + "";
	// 		logger().debug("Souffle command: " + cmd);
	// 		progDbConnection.close();
	// 		FileUtil.run(cmd);
	// 		progDbConnection = SQLUtil.connect(progDbFile.getPath());
	// 	} else {
	// 		Program local = progSplit.getLocalProgram();
	// 		CmdLineOpts internalOpts = new CmdLineOpts();
	// 		internalOpts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
	// 		internalOpts.setSqlDbConnection(progDbConnection);
	// 		internalOpts.setFactsDir(opts.getFactsDir());
	// 		internalOpts.setOutputDir(opts.getOutputDir());
	// 		internalOpts.setDbEntryTag(fileId);

	// 		Compiler.checkProgram(local, internalOpts);
	// 		local.eval(internalOpts);
	// 		local.clearRelations();
	// 	}
	// 	timer.stop();
	// 	profile().stopTimer("local_program", file.getPath());

	// 	logger().time("Running local program on " + file + ":" + timer.getTime() + " ms");
	// }

	// public void runGlobalProgram(CmdLineOpts opts) throws SQLException, IOException {
	// 	logger().debug("Running the global program");

	// 	StopWatch timer = StopWatch.createStarted();
	// 	profile().startTimer("global_program", "total");
	// 	if (useSouffle) {
	// 		String cmd = globalSouffleProg.getPath() + " -D " + opts.getOutputDir()  + " -F " + opts.getFactsDir() +
	// 			" -d " + progDbFile;
	// 		logger().debug("Souffle command: " + cmd);
	// 		progDbConnection.close();
	// 		FileUtil.run(cmd);
	// 		progDbConnection = SQLUtil.connect(progDbFile.getPath());
	// 	} else {
	// 		Program global = progSplit.getGlobalProgram();
	// 		CmdLineOpts internalOpts = new CmdLineOpts();
	// 		internalOpts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
	// 		internalOpts.setSqlDbConnection(progDbConnection);
	// 		internalOpts.setOutputDir(opts.getOutputDir());
	// 		internalOpts.setFactsDir(opts.getFactsDir());
	// 		Compiler.checkProgram(global, internalOpts);
	// 		global.eval(internalOpts);
	// 	}
	// 	timer.stop();
	// 	profile().stopTimer("global_program", "total");

	// 	logger().time("Running the global program: " + timer.getTime() + " ms.");

	// }
}
