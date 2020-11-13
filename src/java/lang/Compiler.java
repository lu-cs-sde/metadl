package lang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;


import org.apache.commons.lang3.time.StopWatch;
import com.fasterxml.jackson.databind.ObjectMapper;

import incremental.IncrementalDriver;
import incremental.ProgramSplit;
import lang.CmdLineOpts.Action;
import lang.ast.AnalyzeBlock;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.SoufflePrettyPrinter;
import lang.ast.StandardPrettyPrinter;
import lang.ast.TypeError;
import lang.ast.SemanticError;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.RelationWrapper;
import swig.SWIGUtil;

/**
 * Dumps the parsed Abstract Syntax Tree of a Calc program.
 */
public class Compiler {
	public static Object DrAST_root_node; // Enable debugging with DrAST

	public static char getCSVSeparator() {
		// TODO: make this a cmd line option
		return ',';
	}

	public static Program parseProgram(CmdLineOpts opts) throws IOException, beaver.Parser.Exception {
		String path = opts.getInputFile();
		StopWatch timer = StopWatch.createStarted();
		Program program = (Program) FileUtil.parse(new File(path));
		Compiler.DrAST_root_node = program; // Enable debugging with DrAST
		timer.stop();
		SimpleLogger.logger().time("Parsing: " + timer.getTime() + "ms");
		return program;
	}

	public static void checkProgram(Program program, CmdLineOpts opts) {
		StopWatch timer = StopWatch.createStarted();
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

	public static void generateIncrementalProgram(Program prog, CmdLineOpts opts) throws IOException, SQLException {
		// evaluate the EDB and IMPORT predicate, to ensure
		// that the inputs to the analyze blocks can be evaluated in turn
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), opts);


		// split program into local and global parts
		ProgramSplit split = new ProgramSplit(prog);
		if (!split.canEvaluateIncrementally()) {
			throw new RuntimeException("Cannot evaluate this program incrementally.");
		}

		IncrementalDriver incDriver = new IncrementalDriver(new File(opts.getOutputDir()), split);
		incDriver.init();

		incDriver.update(opts);

		incDriver.runLocalProgram(opts);
		incDriver.runGlobalProgram(opts);

		incDriver.shutdown();
	}

	public static String getCSVSeparatorEscaped() {
		if (getCSVSeparator() != '\t')
			return String.valueOf(getCSVSeparator());
		return "\\t";
	}

	public static Program run(CmdLineOpts opts) {
		try {
			Program prog = parseProgram(opts);

			switch (opts.getAction()) {
			case EVAL_INTERNAL:
				checkProgram(prog, opts);
				prog.eval(opts);
				break;
			case EVAL_INTERNAL_PARALLEL:
				checkProgram(prog, opts);
				prog.evalParallel(opts);
				break;
			case EVAL_SOUFFLE:
				checkProgram(prog, opts);
				prog.evalEDB(prog.evalCtx(), opts);
				prog.evalIMPORT(prog.evalCtx(), opts);
				prog.generateObjectProgramRelations(opts);
				evalSouffleProgram(prog, opts);
				break;
			case PRETTY_SOUFFLE:
				checkProgram(prog, opts);
				prog.evalEDB(prog.evalCtx(), opts);
				prog.evalIMPORT(prog.evalCtx(), opts);
				prettyPrintSouffle(prog, opts.getOutputDir() + "/" + opts.getOutputFile());
				break;
			case PRETTY_INTERNAL:
				StandardPrettyPrinter<Program> spp = new StandardPrettyPrinter<>(new PrintStream(System.out));
				spp.prettyPrint(prog);
				checkProgram(prog, opts);
				break;
			case PRETTY_TYPES:
				prog.dumpTypes(System.out);
				prog.dumpDomainSignatures(System.out);
				checkProgram(prog, opts);
				break;
			case EVAL_IMPORT:
				// TODO: rename EVAL_IMPORT to EVAL_ANALYZE_BLOCKS or just remove the option
				checkProgram(prog, opts);
				prog.evalEDB(prog.evalCtx(), opts);
				prog.evalIMPORT(prog.evalCtx(), opts);
				prog.generateObjectProgramRelations(opts);
				break;
			case CHECK:
				checkProgram(prog, opts);
				prog.dumpStrata();
				prog.clauseDependencyGraph().dump();
				break;
			case GEN_HYBRID:
				checkProgram(prog, opts);
				prog.evalEDB(prog.evalCtx(), opts);
				prog.evalIMPORT(prog.evalCtx(), opts);
				generateSouffleSWIGProgram(prog, opts);
				break;
			case EVAL_HYBRID:
				checkProgram(prog, opts);
				SWIGUtil.evalHybridProgram(prog, opts);
				break;
			case INCREMENTAL_UPDATE:
			case INCREMENTAL_INIT:
				checkProgram(prog, opts);
				generateIncrementalProgram(prog, opts);
				break;
			}

			return prog;
		} catch (IOException e) {
			throw new RuntimeException(e);
		} catch (SQLException e) {
			throw new RuntimeException(e);
		} catch (beaver.Parser.Exception e) {
			System.err.println("Failed to parse the input program\n" + e.toString());
			throw new RuntimeException(e);
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
		run(opts);

		totalTime.stop();
		SimpleLogger.logger().time("Total: " + totalTime.getTime() + "ms ");
	}
}
