package lang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.time.StopWatch;

import incremental.IncrementalDriver;
import incremental.ProgramSplit;
import lang.CmdLineOpts.Action;
import lang.ast.AnalyzeBlock;
import lang.ast.FormalPredicate;
import lang.ast.HFPProgram;
import lang.ast.Program;
import lang.ast.SoufflePrettyPrinter;
import lang.ast.StandardPrettyPrinter;
import lang.ast.TypeError;
import lang.ast.SemanticError;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.RelationWrapper;
import static prof.Profile.profile;
import swig.SWIGUtil;

/**
 * Dumps the parsed Abstract Syntax Tree of a Calc program.
 */
public class Compiler {
	public static Object DrAST_root_node; // Enable debugging with DrAST

	public static final long START = System.currentTimeMillis(); // FIXME: remove
	public static char getCSVSeparator() {
		// TODO: make this a cmd line option
		return ',';
	}

	public static Program parseProgram(String owner_module, CmdLineOpts opts) throws IOException, beaver.Parser.Exception {
		// try {
		// 	throw new RuntimeException("Who called parseProgram()?");
		// } catch (RuntimeException exn) {
		// 	exn.printStackTrace();
		// }
		String path = opts.getInputFile();
		StopWatch timer = StopWatch.createStarted();
		profile().startTimer(owner_module, "parsing");
		final String file_body =  FileUtils.readFileToString(new File(path), StandardCharsets.UTF_8);
		Program program = (Program) FileUtil.parse(file_body);
		if (program != null) {
			program.setProgramSourceString(file_body);
		}
		Compiler.DrAST_root_node = program; // Enable debugging with DrAST
		timer.stop();
		profile().stopTimer(owner_module, "parsing");
		SimpleLogger.logger().time("Parsing: " + timer.getTime() + "ms");
		// always print so we can see if hfp failed
		System.out.println("Parsing completed in " + timer.getTime() + "ms [=> no hfp]");
		return program;
	}

	public static void checkProgram(Program program, CmdLineOpts opts) {
		StopWatch timer = StopWatch.createStarted();
		profile().startTimer("main", "sema_and_type_check");
		if (program.hasSemanticErrors()) {
			System.err.println(program.errorReport());
			System.err.println("Compilation failed with semantic errors.");
			throw new RuntimeException();
		}

		if (!program.typeErrors().isEmpty()) {
			for (TypeError e : program.typeErrors())
				System.err.println(e.reportPosition());
			System.err.println("Compilation failed with type errors.");
			throw new RuntimeException();
		}

		if (opts.isWarningsEnabled() && !program.semanticWarnings().isEmpty()) {
			for (SemanticError e : program.semanticWarnings()) {
				System.err.println(e.reportPosition());
			}
		}
		profile().stopTimer("main", "sema_and_type_check");
		timer.stop();
		SimpleLogger.logger().time("Semantic and type analysis: " + timer.getTime() + "ms");
	}

	public static void prettyPrintSouffle(Program program, String soufflePath) throws FileNotFoundException {

		SimpleLogger.logger().log("PrettyPrint to: " + soufflePath, SimpleLogger.LogLevel.Level.DEBUG);

		StopWatch souffleGenTimer = StopWatch.createStarted();
		program.soufflePrint(new SoufflePrettyPrinter<Program>(new PrintStream(soufflePath)));
		souffleGenTimer.stop();
		SimpleLogger.logger().time("Generate Souffle program: " + souffleGenTimer.getTime() + "ms");
	}

	public static void evalSouffleProgram(Program program, CmdLineOpts opts) throws IOException {
		String soufflePath = opts.getOutputFile();
		prettyPrintSouffle(program, soufflePath);

		// Disable warnings in souffle
		String cmd = "souffle -w -D " + opts.getOutputDir() + " " + soufflePath + " -F " + opts.getFactsDir();
		SimpleLogger.logger().log("Run souffle with: " + cmd, SimpleLogger.LogLevel.Level.DEBUG);

		StopWatch souffleRunTimer = StopWatch.createStarted();
		FileUtil.run(cmd);
		souffleRunTimer.stop();
		SimpleLogger.logger().time("Run Souffle program: " + souffleRunTimer.getTime() + "ms");
	}

	public static void generateSouffleSWIGProgram(Program program, CmdLineOpts opts) throws IOException {
		String soufflePath = opts.getOutputFile();
		prettyPrintSouffle(program, soufflePath);

		// Disable warnings in souffle, provide -j 4 to enable the use of parallel
		// loops (the number of thread is irrelevant as long as it's greater than 1)
		String cmd = "souffle -j 4 -s java -w -p " + FileUtil.fileNameNoExtension(soufflePath) + ".prof " + soufflePath;
		SimpleLogger.logger().log("Run souffle with: " + cmd, SimpleLogger.LogLevel.Level.DEBUG);

		StopWatch souffleRunTimer = StopWatch.createStarted();
		FileUtil.run(cmd);
		souffleRunTimer.stop();
		SimpleLogger.logger().time("Run Souffle program: " + souffleRunTimer.getTime() + "ms");
	}

	/**
	 * Emit HybridFastPath summary code (or delete code file if fast path not feasible)
	 *
	 * @param lib_name Name of the Soufflé library file ("/path/foo.so") relative to which to generate
	 */
	public static void generateSouffleHFPSummary(Program program, String lib_name) throws IOException {
		writeSouffleHFPSummary(program.getHFPProgram(), lib_name);
	}

	/**
	 * Emit HybridFastPath summary code (or delete code file if fast path not feasible)
	 *
	 * @param lib_name Name of the Soufflé library file ("/path/foo.so") relative to which to generate
	 */
	public static void writeSouffleHFPSummary(HFPProgram hfprog, String lib_name) throws IOException {
		final String summaryFileName = FileUtil.filePathNoExtension(lib_name) + HFPProgram.FILE_SUFFIX;
		if (hfprog == null) {
			// fast path not feasible; make sure any obsolete fastpath program is deleted
			SimpleLogger.logger().log("Cannot get HybridFastPath summary => making sure to remove any existing '" + summaryFileName + "'");
			Files.deleteIfExists(Paths.get(summaryFileName));
		} else {
			SimpleLogger.logger().log("Emitting HybridFastPath summary to '" + summaryFileName + "'");
			Files.write(Paths.get(summaryFileName), hfprog.serialize().getBytes());
		}
	}

	/**
	 * @param lib_name Name of the Soufflé library file ("/path/foo.so") relative to which to load
	 */
	public static HFPProgram loadSouffleHFPSummary(String lib_name, CmdLineOpts opts) {
		final String summaryFileName = FileUtil.filePathNoExtension(lib_name) + HFPProgram.FILE_SUFFIX;
		if (!opts.isHFPEnabled()) {
			SimpleLogger.logger().log("HybridFastPath explicitly disabled via command line option, not attempting to work with '" + summaryFileName + "'");
			return null;
		}
		Path file = Paths.get(summaryFileName);
		if (Files.exists(file) && Files.isReadable(file)) {
			try {
				final String file_body =  FileUtils.readFileToString(file.toFile(), StandardCharsets.UTF_8);
				HFPProgram prog = HFPProgram.deserialize(file_body);
				SimpleLogger.logger().log("Loaded '" + summaryFileName + "', will use HybridFastPath [hfp]");
				return prog;
			} catch (IOException exn) {
				exn.printStackTrace();
				SimpleLogger.logger().log("HybridFastPath [hfp] error: failed to deserialize '" + summaryFileName +  "': " + exn);
				return null;
			}
		}
		SimpleLogger.logger().log("No '" + summaryFileName + "' found, will not try HybridFastPath [hfp]");
		return null;
	}

	public static void generateIncrementalProgram(CompilerInput cinput, CmdLineOpts opts, boolean useSouffle) throws IOException, SQLException {
		// evaluate the EDB and IMPORT predicate, to ensure
		// that the inputs to the analyze blocks can be evaluated in turn

		// CR: postponed
		// prog.evalEDB(prog.evalCtx(), opts);
		// prog.evalIMPORT(prog.evalCtx(), opts);


		cinput.setTimerParent("incremental_driver");
		profile().startTimer("main", "incremental_driver");
		IncrementalDriver incDriver = new IncrementalDriver(new File(opts.getCacheDir()), cinput, useSouffle);
		incDriver.init();

		incDriver.update();

		incDriver.shutdown();

		profile().stopTimer("main", "incremental_driver");
		cinput.setTimerParent("main");
	}

	public static String getCSVSeparatorEscaped() {
		if (getCSVSeparator() != '\t')
			return String.valueOf(getCSVSeparator());
		return "\\t";
	}

	public static CompilerInput run(CmdLineOpts opts) {
		try {
			Program prog = null;
			CompilerInput cinput = new CompilerInput(opts);

			switch (opts.getAction()) {
			case EVAL_INTERNAL:
				cinput.getProgram().eval(opts);
				break;
			case EVAL_INTERNAL_PARALLEL:
				cinput.getProgram().evalParallel(opts);
				break;
			case EVAL_SOUFFLE:
				prog = cinput.getProgram();
				cinput.evalIDBandIMPORT();
				prog.generateObjectProgramRelations(opts);
				evalSouffleProgram(prog, opts);
				break;
			case PRETTY_SOUFFLE:
				prog = cinput.getProgram();
				cinput.evalIDBandIMPORT();
				prettyPrintSouffle(prog, opts.getOutputDir() + "/" + opts.getOutputFile());
				break;
			case PRETTY_INTERNAL:
				prog = cinput.getUncheckedProgram();
				StandardPrettyPrinter<Program> spp = new StandardPrettyPrinter<>(new PrintStream(System.out));
				spp.prettyPrint(prog);
				cinput.checkProgram();
				break;
			case PRETTY_TYPES:
				prog = cinput.getUncheckedProgram();
				prog.dumpTypes(System.out);
				prog.dumpDomainSignatures(System.out);
				cinput.checkProgram();
				break;
			case EVAL_IMPORT:
				// TODO: rename EVAL_IMPORT to EVAL_ANALYZE_BLOCKS or just remove the option
				prog = cinput.getProgram();
				cinput.evalIDBandIMPORT();
				prog.generateObjectProgramRelations(opts);
				break;
			case CHECK:
				prog = cinput.getProgram();
				prog.dumpStrata();
				prog.clauseDependencyGraph().dump();
				break;
			case GEN_HYBRID:
				prog = cinput.getProgram();
				cinput.evalIDBandIMPORT();
				generateSouffleSWIGProgram(prog, opts);
				if (opts.isHFPEnabled()) {
					generateSouffleHFPSummary(prog, opts.getLibFile());
				}
				break;
			case EVAL_HYBRID:
				HFPProgram fastpath = loadSouffleHFPSummary(opts.getLibFile(), opts);
				if (fastpath != null) {
					SWIGUtil.evalHFPProgram(fastpath, opts);
				} else {
					SWIGUtil.evalHybridProgram(cinput.getProgram(), opts);
				}
				break;
			case INCREMENTAL_UPDATE:
			case INCREMENTAL_INIT:
				generateIncrementalProgram(cinput, opts, true);
				break;
			case INCREMENTAL_INIT_INTERNAL:
			case INCREMENTAL_UPDATE_INTERNAL:
				generateIncrementalProgram(cinput, opts, false);
				break;
			}

			return cinput;
		} catch (IOException e) {
			throw new RuntimeException(e);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		} catch (RuntimeException exn) {
			if (exn.getCause() instanceof beaver.Parser.Exception) {
				System.err.println("Failed to parse the input program\n" + exn.getCause().toString());
			}
			throw exn;
		}
	}

	public static void main(String[] args) {
		// try {
		// 	Thread.sleep(20 * 1000);
		// } catch (InterruptedException e) {
		// }

		Logger log = Logger.getLogger("lang.io");
		log.setLevel(Level.FINEST);
		StopWatch totalTime = StopWatch.createStarted();

		CmdLineOpts opts = CmdLineOpts.parseCmdLineArgs(args);
		if (opts.getProfileFile() != null) {
			// set the output file  for the profiling information
			profile().setOutput(new File(opts.getProfileFile()));
		}

		profile().startTimer("main", "total");
		run(opts);
		profile().stopTimer("main", "total");

		totalTime.stop();
		SimpleLogger.logger().time("Total: " + totalTime.getTime() + "ms ");

		if (opts.getProfileFile() != null) {
			// write out the profiling information
			profile().writeOut();
		}
	}

}
