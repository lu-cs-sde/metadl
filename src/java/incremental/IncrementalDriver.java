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
import lang.CompilerInput;
import lang.ast.AnalyzeBlock;
import lang.ast.FormalPredicate;
import lang.ast.HFPProgram;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.ProgramRepresentation;
import lang.ast.StandardPrettyPrinter;
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
	private File baseHFPProgram;

	private CompilerInput cinput;

	private boolean useSouffle;

	// HybridFastPath program, if available (generated during init() if needed
	private HFPProgram hfpBase = null; // on-demand load via getHFPBase() (this is the un-split program)
	private HFPProgram.Runner hfpBaseRunner = null; // HFPProgram.Program runner exposes predicate information

	private boolean hfpBaseTriedLoad = false;

	public IncrementalDriver(File root, CompilerInput cinput, boolean useSouffle) {
		this.useSouffle = useSouffle;
		this.cinput = cinput;
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
		this.baseHFPProgram = new File(prog, "basehfp" + HFPProgram.FILE_SUFFIX);
	}

	private FileIdDatabase fileIdDb;
	private Map<String, String> externalClasses = new TreeMap<>();

	private HFPProgram
	getHFPBase() {
		if (this.hfpBase == null && !this.hfpBaseTriedLoad) {
			this.hfpBaseTriedLoad = true;
			this.hfpBase = Compiler.loadSouffleHFPSummary(this.baseHFPProgram.getPath(), this.cinput.getOpts());
		}
		return this.hfpBase;
	}

	private boolean gen_timer_started = false;
	private boolean gen_timer_started_with_split = false;
	/**
	 * Starts the program generation timer, optionally after producing a Program Split.
	 *
	 * Can be called more than once, but not if the first call did NOT request a ProgramSplit.
	 *
	 * This avoids timer overlap and simplifies the timing logic in init().
	 */
	private ProgramSplit
	startProgramGenerationTimer(boolean get_program_split) {
		ProgramSplit retval = null;
		if (get_program_split) {
			if (this.gen_timer_started && !this.gen_timer_started_with_split) {
				throw new RuntimeException("Invalid use would mess up timing");
			}
			retval = this.cinput.getProgramSplit();
		}
		if (!this.gen_timer_started) {
			profile().startTimer("incremental_driver", "generate_programs");
			this.gen_timer_started = true;
			this.gen_timer_started_with_split = get_program_split;
		}
		return retval;
	}

	private void
	stopProgramGenerationTimer() {
		if (!gen_timer_started) {
			this.startProgramGenerationTimer(false);
		}
		profile().stopTimer("incremental_driver", "generate_programs");
		gen_timer_started = false;
	}

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
		File update_prog = new File(prog, "update.mdl");
		File fused_prog = new File(prog, "fused.mdl");

		// Start program generation timer as late as possible to avoid overlap with
		// parsing / program analysis

		if (this.cinput.isOutOfDate(update_prog)
		    || this.cinput.isOutOfDate(fused_prog)) {
			ProgramSplit progSplit = this.startProgramGenerationTimer(true);
			dumpProgram(progSplit.getUpdateProgram(), update_prog);
			dumpProgram(progSplit.getFusedProgram(), fused_prog);
		}

		if (useSouffle) {
			// update HFP program as approrpiate
			if (this.cinput.getOpts().isHFPEnabled()
			    && this.cinput.isOutOfDate(this.baseHFPProgram)) {
				ProgramSplit progSplit = this.startProgramGenerationTimer(true);

				this.hfpBase = null;
				Collection<String> cached_predicates = this.getCachedPredicates();

				this.hfpBase = this.cinput.getProgram().getHFPProgram();
				if (this.hfpBase != null) {
					this.hfpBase.setAnnotation(HFPProgram.ANNOTATION_SOURCE_RELATION, progSplit.getSourceRelation().getPRED_ID());
					this.hfpBase.setCachedPredicates(progSplit.getFusedProgram(),
									 cached_predicates);
				}
				// We do this even when hfpBase is null to generate a log message
				Compiler.writeSouffleHFPSummary(this.hfpBase, baseHFPProgram.getPath());
			}

			if (this.cinput.isOutOfDate(fusedSouffleLib)) {
				ProgramSplit progSplit = this.startProgramGenerationTimer(true);

				File tmp = new File(prog, "fused.dl");
				Compiler.prettyPrintSouffle(progSplit.getFusedProgram(), tmp.getPath());
				compileSouffleLib(tmp, fusedSouffleLib);
			}

			if (this.cinput.isOutOfDate(updateSouffleProg)) {
				ProgramSplit progSplit = this.startProgramGenerationTimer(true);

				generateSouffleProgram(progSplit.getUpdateProgram(), updateSouffleProg, "-j 4 -p update.profile");
			} else {
				this.startProgramGenerationTimer(false);
				logger().info("Using existing Souffle update program from " + updateSouffleProg);
			}

		}

		this.stopProgramGenerationTimer();
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

	private Collection<String>
	getCachedPredicates() {
		HFPProgram base = this.getHFPBase();
		if (base != null) {
			return base.getCachedPredicates();
		}
		return this.cinput.getProgramSplit().getCachedPredicates();
	}

	private int
	realArity(String predicate_name) {
		HFPProgram base = this.getHFPBase();
		if (base != null) {
			if (this.hfpBaseRunner == null) {
				throw new RuntimeException("Violated assumption: hfpBaseRunner (partly) run before we start deleting (should be done by computeAnalyzedSources())");
			}
			return this.hfpBaseRunner.type(predicate_name).arity();
		}
		FormalPredicate p = this.cinput.getProgramSplit().getFusedProgram().formalPredicateMap().get(predicate_name);
		return p.realArity();
	}

	private void delete(List<File> sourceFiles, String relPrefix) throws SQLException {
		//ProgramSplit progSplit = this.cinput.getProgramSplit();
		for (File f : sourceFiles) {
			int fileId = fileIdDb.getIdForFile(f.getPath());

			for (String localTable : this.getCachedPredicates()) {
				// cached predicates are extended to the right with a tag field
				int tagIndex = this.realArity(localTable);
				deleteEntries(progDbConnection, localTable, tagIndex, fileId);
			}
		}

		progDbConnection.commit();
	}

	private String
	getSourceRelationName() {
		HFPProgram base = this.getHFPBase();
		if (base != null) {
			return base.getAnnotation(HFPProgram.ANNOTATION_SOURCE_RELATION);
		}
		return this.cinput.getProgramSplit().getSourceRelation().getPRED_ID();
	}

	private RelationWrapper computeAnalyzedSources(String factDir) throws IOException, SQLException {
		if (this.getHFPBase() != null) {
			return this.computeAnalyzedSourcesHFP(factDir);
		} else {
			return this.computeAnalyzedSourcesUnoptimized(factDir);
		}
	}


	private HFPProgram.Runner
	getHFPBaseRunner() {
		HFPProgram hfpBase = this.getHFPBase();
		HFPProgram.Runner runner = this.hfpBaseRunner;
		if (runner == null) {
			runner = hfpBase.runner(null, this.cinput.getOpts());
			this.hfpBaseRunner = runner;
			// Execute everything, except for the analyze blocks themselves, which we handle
			// manually in `update()'
			try {
				runner.runUntil(HFPProgram.ANALYZE);
			} catch (IOException | SQLException exn) {
				throw new RuntimeException(exn);
			}
		}
		return runner;
	}

	private RelationWrapper computeAnalyzedSourcesHFP(String factDir) throws IOException, SQLException {
		final String srcPredName = hfpBase.getAnnotation(HFPProgram.ANNOTATION_SOURCE_RELATION);
		return this.getHFPBaseRunner().getRelationWrapper(srcPredName);
	}

	private RelationWrapper computeAnalyzedSourcesUnoptimized(String factDir) throws IOException, SQLException {
		Program prog = this.cinput.getProgram();
		CmdLineOpts opts = new CmdLineOpts();
		opts.setAction(CmdLineOpts.Action.EVAL_INTERNAL);
		opts.setFactsDir(factDir);

		// evaluate the EDB and IMPORT predicate, to ensure
		// that the inputs to the analyze blocks can be evaluated in turn
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), opts);

		// find out which predicate defines the analyzed sources
		String srcPredName = this.getSourceRelationName();
		FormalPredicate srcPred = this.cinput.getProgram().formalPredicateMap().get(srcPredName);
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

	private SWIGSouffleProgram swigSouffleProgram = null;
	private Map<String, TupleInserter> swigTupleInserter = null;

	private void
	loadSWIG() {
		if (this.swigSouffleProgram != null) {
			return;
		}

		if (this.getHFPBase() != null) {
			// fast path
			Pair<SWIGSouffleProgram, Map<String, TupleInserter>> swigProg =
				SWIGUtil.loadSWIGProgramStringLinkage(this.getHFPBase().getFormalPredicates(), fusedSouffleLib.getAbsoluteFile(), "fused");
			this.swigSouffleProgram = swigProg.getLeft();
			this.swigTupleInserter = swigProg.getRight();
			return;
		}
		// slow path
		Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> swigProg =
			SWIGUtil.loadSWIGProgram(this.cinput.getProgramSplit().getFusedProgram().getFormalPredicates(), fusedSouffleLib.getAbsoluteFile(), "fused");
		this.swigSouffleProgram = swigProg.getLeft();
		this.swigTupleInserter = swigProg.getRight().entrySet().stream()
			.collect(Collectors.toMap(e -> e.getKey().getPRED_ID(), e -> e.getValue()));
	}

	private Map<String, TupleInserter>
	getSWIGTupleInserter() {
		loadSWIG();
		return this.swigTupleInserter;
	}

	private SWIGSouffleProgram
	getSWIGProgram() {
		loadSWIG();
		return this.swigSouffleProgram;
	}


	/**
	   Update the program representation for the given files.
	 */
	public void update() throws IOException, SQLException {

		profile().startTimer("incremental_driver", "update");
		final CmdLineOpts opts = this.cinput.getOpts();
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
				// No fast path available without Soufflé
				ProgramSplit progSplit = this.cinput.getProgramSplit();
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


		Map<String, TupleInserter> tupleInsertersByName = this.getSWIGTupleInserter();

		// populate the program representation relations for each analyze block
		// first collect the prefix names for all analyze blocks
		List<String> analyze_prefixes = new ArrayList<>();
		if (this.getHFPBase() != null) {
			// fast path
			HFPProgram hfprog = this.getHFPBase();
			HFPProgram.Runner runner = this.getHFPBaseRunner();
			while (!runner.isDone()) {
				HFPProgram.AnalyzeInfo analyze = runner.stepOverAnalyze();
				if (analyze == null) {
					continue;
				}
				analyze_prefixes.add(analyze.getPrefix());
			}
		} else {
			// slow path
			ProgramSplit progSplit = this.cinput.getProgramSplit();
			for (AnalyzeBlock b : progSplit.getProgram().analyzeBlocks()) {
				analyze_prefixes.add(b.getContext().scopePrefix);
			}
		}

		// now emit program relations for all these prefixes
		for (String prefix : analyze_prefixes) {
			if (opts.getAction() == CmdLineOpts.Action.INCREMENTAL_UPDATE) {
				// remove the information for the deleted files, but also for the files
				// that need revisiting
				profile().startTimer("incremental_driver", "delete_facts");
				delete(Stream.concat(deletedFiles.stream(), visitFiles.stream()).collect(Collectors.toList()),
				       prefix);
				profile().stopTimer("incremental_driver", "delete_facts");
			}
			HybridDatalogProjectionSink sink = new HybridDatalogProjectionSink();
			// go over the program representation relations in this analyze block and figure
			// out whether they are used in the fused progam, if they are, then set the proper
			// inserters in the sink
			for (ProgramRepresentation pr : ProgramRepresentation.values()) {
				String predName = prefix + pr.getPredicateName();
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

			progDbConnection.close();
			runHybridProgram(this.getSWIGProgram(), opts);
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
