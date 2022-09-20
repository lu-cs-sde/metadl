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
import lang.ast.FormalPredicate;
import lang.ast.MapEntry;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.ProgramRepresentation;
import lang.ast.StandardPrettyPrinter;
import lang.ast.StringType;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SQLUtil;
import lang.Compiler;
import static lang.io.SimpleLogger.*;
import static prof.Profile.profile;
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
		profile().startTimer("incremental_driver", "init");

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

		profile().stopTimer("incremental_driver", "init");

		profile().startTimer("incremental_driver", "generate_programs");
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

		profile().stopTimer("incremental_driver", "generate_programs");
	}

	public void shutdown() throws IOException, SQLException {
		profile().startTimer("incremental_driver", "shutdown");
		// store the file-id database
		fileIdDb.storeToTable(progDbConnection, ProgramSplit.FILE_ID);
		// store the external classes file
		CSVUtil.writeMap(externalClasses, externalClassesFile.getPath());
		// the connection to the program db is closed only once for each
		// incremental run
		progDbConnection.close();
		profile().stopTimer("incremental_driver", "shutdown");
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

	private static synchronized void compileSouffleLib(File src, File exec) throws IOException {
		String cmd = "souffle -j 4 -s java -w -p " + FileUtil.fileNameNoExtension(src.getPath()) + ".prof " + src.getPath();
		logger().debug("Compiling Souffle library with: " + cmd);
		FileUtil.run(cmd);
		// The output of the command is always a file called libSwigInterface.so. This method
		// is synchronized to be able to run two incremental runs in parallel (for example in the
		// test suite).
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

	private Collection<ClassSource> generateFromSrc(File f,
													CompilationUnit cu,
													DatalogProjection2 proj,
													DatalogProjectionSink sink) throws IOException {
		List<ClassSource> externalClasses = new ArrayList<>();

		logger().debug("Extracting AST facts from source " + f.getPath() + ".");

		profile().startTimer("object_file_generate_relations", f.getPath());

		Set<CompilationUnit> externalCUs = proj.generate(cu, sink);
		for (CompilationUnit u : externalCUs) {
			if (!u.fromSource()) {
				ClassSource src = u.getClassSource();
				externalClasses.add(src);
			}
		}
		// externalCUs.stream().filter(u -> !u.fromSource())
		// 	.map(CompilationUnit::getClassSource).forEachOrdered(externalClasses::add);

		profile().stopTimer("object_file_generate_relations", f.getPath());

		return externalClasses;
	}

	private Collection<ClassSource> generateFromLib(ClassSource src,
													org.extendj.ast.Program externalClassProgram,
													DatalogProjection2 proj,
													DatalogProjectionSink sink) throws IOException {
		// this class file is not in the database
		logger().debug("Adding class file to database " + src.relativeName() + ".");
		src.openInputStream();

		profile().startTimer("object_file_compile", src.relativeName());
		CompilationUnit externalCU = src.parseCompilationUnit(externalClassProgram);
		profile().stopTimer("object_file_compile", src.relativeName());

		profile().startTimer("object_file_generate_relations", src.relativeName());


		proj.generate(externalCU, sink);
		profile().stopTimer("object_file_generate_relations", src.relativeName());

		return Collections.emptyList();
	}

	/**
	   Generate the program representation for the given source files, and store it
	   to predicates with a given prefix
	 */
	private void generate(List<File> sourceFiles, DatalogProjectionSink sink) throws IOException {
		org.extendj.ast.Program p = createProgram(fileIdDb);
		DatalogProjection2 proj = new DatalogProjection2(fileIdDb, p.provenance);

		Set<ClassSource> externalClasses = new TreeSet<>(new Comparator<ClassSource>() {
				@Override
				public int compare(ClassSource arg0, ClassSource arg1) {
					return arg0.relativeName().compareTo(arg1.relativeName());
				}
			});

		List<Pair<File, CompilationUnit>> srcCUs = new ArrayList<>();
		// build a program from all the CUs; all the other CUs that are loaded on-demand
		// (e.g. from imports), will be shared
		for (File f : sourceFiles) {
			if (!f.exists()) {
				logger().info("Skipping file " + f + ". File does not exist. ");
				continue;
			}
			profile().startTimer("object_file_compile", f.getPath());
			CompilationUnit cu = p.addSourceFile(f.getPath());
			profile().stopTimer("object_file_compile", f.getPath());
			srcCUs.add(Pair.of(f, cu));
		}
		checkProgram(p);

		// Now generate the program representation only for the added sources
		for (Pair<File, CompilationUnit> pair : srcCUs) {
			externalClasses.addAll(generateFromSrc(pair.getLeft(), pair.getRight(), proj, sink));
		}

		for (ClassSource src : externalClasses) {
			if (this.externalClasses.put(src.relativeName(), src.relativeName()) != null) {
				// check whether the external class was already analyzed
				logger().debug("Skipping external class " + src.relativeName());
				continue;
			}

			generateFromLib(src, p, proj, sink);
		}
	}

	private static void deleteEntries(Connection conn, String table, int tagIndex, int fileId) throws SQLException {
		Statement stmt = conn.createStatement();
		stmt.executeUpdate("PRAGMA secure_delete=OFF");
		stmt.executeUpdate("CREATE INDEX IF NOT EXISTS " + table + "_index ON " + table + "('" + tagIndex + "')");
		conn.commit();
		stmt.executeUpdate("DELETE FROM '" + table + "' WHERE '" + table + "'.'" + tagIndex + "' = " + fileId);
	}

	private void delete(List<File> sourceFiles) throws SQLException {
		for (File f : sourceFiles) {
			int fileId = fileIdDb.getIdForFile(f.getPath());

			for (String localTable  : progSplit.getCachedPredicates()) {
				FormalPredicate p = progSplit.getFusedProgram().formalPredicateMap().get(localTable);
				// cached predicates are extended to the right with a tag field
				int tagIndex = p.realArity();
				deleteEntries(progDbConnection, localTable, tagIndex, fileId);
			}
		}

		progDbConnection.commit();
	}

	private RelationWrapper computeAnalyzedSources(CmdLineOpts opts) throws IOException, SQLException {
		Program prog = progSplit.getProgram();

		// copy the tuples from one relation to another
		RelationWrapper srcWrapper = new RelationWrapper(prog.evalCtx(), new Relation2(2), new PredicateType(StringType.get(), StringType.get()));

		for (Map.Entry<String, String> src : opts.getSrcs().entrySet()) {
			srcWrapper.insertTuple(src.getKey(), src.getValue());
		}

		return srcWrapper;
	}

	private static void setRelation(Program prog, String relName, RelationWrapper rel) {
		FormalPredicate pred = prog.formalPredicateMap().get(relName);
		EvaluationContext ctx  = prog.evalCtx();
		ctx.getRelation(pred).clear();
		RelationWrapper dst = new RelationWrapper(ctx, ctx.getRelation(pred), pred.type());
		dst.insertTuples(rel.tuples());
	}

	private static RelationWrapper getRelation(Program prog, String relName) {
		FormalPredicate pred = prog.formalPredicateMap().get(relName);
		EvaluationContext ctx  = prog.evalCtx();
		RelationWrapper src = new RelationWrapper(ctx, ctx.getRelation(pred), pred.type());
		return src;
	}


	/**
	   Update the program representation for the given files.
	 */
	public void update(CmdLineOpts opts) throws IOException, SQLException {
		profile().startTimer("incremental_driver", "update");
		RelationWrapper analyzedSrcs = computeAnalyzedSources(opts);

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
				SQLUtil.clearRelation(progDbConnection, ProgramSplit.AST_VISIT_RELATION);
				SQLUtil.clearRelation(progDbConnection, ProgramSplit.AST_REMOVE_RELATION);

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

		if (opts.getAction() == CmdLineOpts.Action.INCREMENTAL_UPDATE) {
			// remove the information for the deleted files, but also for the files
			// that need revisiting
			profile().startTimer("incremental_driver", "delete_facts");
			delete(Stream.concat(deletedFiles.stream(), visitFiles.stream()).collect(Collectors.toList()));
			profile().stopTimer("incremental_driver", "delete_facts");
		}


		HybridDatalogProjectionSink sink = new HybridDatalogProjectionSink();
		// go over the program representation relations in this analyze block and figure
		// out whether they are used in the fused progam, if they are, then set the proper
		// inserters in the sink
		for (ProgramRepresentation pr : ProgramRepresentation.values()) {
			String predName = progSplit.getProgram().getAnalysisContext().prefix(pr.getPredicateName());
			TupleInserter ti = tupleInsertersByName.get(predName);
			if (ti != null) {
				logger().debug("Writing directly to the souffle relation " + predName);
				sink.setTupleInserter(pr, ti);
			}
		}

		generate(visitFiles, sink);


		// now run the hybrid program
		{
			// Program fusedProgram = progSplit.getFusedProgram();
			// fusedProgram.evalEDB(fusedProgram.evalCtx(), opts);
			// fusedProgram.evalIMPORT(fusedProgram.evalCtx(), opts);

			progDbConnection.close();
			runHybridProgram(swigProg.getLeft(), opts);
			progDbConnection = SQLUtil.connect(progDbFile.getPath());
		}

		profile().stopTimer("incremental_driver", "update");
	}

	private void runHybridProgram(SWIGSouffleProgram swigProg, CmdLineOpts opts) {
		int hwThreads = Math.max(Runtime.getRuntime().availableProcessors() / 2, 1);
		swigProg.setNumThreads(hwThreads);
		profile().startTimer("hybrid_program", "load");
		swigProg.loadAll(opts.getFactsDir(), progDbFile.getPath());
		profile().stopTimer("hybrid_program", "load");

		profile().startTimer("hybrid_program", "run");
		swigProg.run();
		profile().stopTimer("hybrid_program", "run");

		profile().startTimer("hybrid_program", "print");
		swigProg.printAll(opts.getOutputDir(), progDbFile.getPath());
		profile().stopTimer("hybrid_program", "print");

		swigProg.delete();
	}
}
