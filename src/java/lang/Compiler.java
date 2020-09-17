package lang;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.tools.ToolProvider;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.CommandLine;

import lang.CmdLineOpts.Action;
import lang.ast.Program;
import lang.ast.SoufflePrettyPrinter;
import lang.ast.StandardPrettyPrinter;
import lang.ast.TypeError;
import lang.ast.SemanticError;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
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

	public static void printHelp(Options options) {
		String header = "Compile and run a MetaDL program.\n\n";
		String footer = "\nPlease report issues at https://github.com/lu-cs-sde/metadl";

		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("metadl PROGRAM", header, options, footer, true);
	}

	public static Program parseAndCheckProgram(CmdLineOpts opts) throws IOException, beaver.Parser.Exception {
		String path = opts.getInputFile();
		StopWatch timer = StopWatch.createStarted();
		Program program = (Program) FileUtil.parse(new File(path));
		Compiler.DrAST_root_node = program; // Enable debugging with DrAST

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
		SimpleLogger.logger().time("Parsing and checking program: " + timer.getTime() + "ms");
		return program;
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
		Process p = Runtime.getRuntime().exec(cmd);

		BufferedReader brerr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));

		while (p.isAlive()) {
			while (brerr.ready())
				SimpleLogger.logger().log(":SOUFFLE-ERROR " + brerr.readLine(), SimpleLogger.LogLevel.Level.ERROR);
			while (br.ready())
				SimpleLogger.logger().log(":SOUFFLE-OUTPUT " + br.readLine(), SimpleLogger.LogLevel.Level.DEBUG);
		}

		try {
			p.waitFor();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
		souffleRunTimer.stop();
		SimpleLogger.logger().time("Run Souffle program: " + souffleRunTimer.getTime() + "ms");

		br.close();
		brerr.close();
	}

	public static void generateSouffleSWIGProgram(Program program, CmdLineOpts opts) throws IOException {
		String soufflePath = opts.getOutputFile();
		prettyPrintSouffle(program, soufflePath);

		// Disable warnings in souffle, provide -j 4 to enable the use of parallel
		// loops (the number of thread is irrelevant as long as it's greater than 1)
		String cmd = "souffle -j 4 -s java -w -p " + FileUtil.fileNameNoExtension(soufflePath) + ".prof " + soufflePath;
		SimpleLogger.logger().log("Run souffle with: " + cmd, SimpleLogger.LogLevel.Level.DEBUG);

		StopWatch souffleRunTimer = StopWatch.createStarted();
		Process p = Runtime.getRuntime().exec(cmd);

		BufferedReader brerr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));

		while (p.isAlive()) {
			while (brerr.ready())
				SimpleLogger.logger().log(":SOUFFLE-ERROR " + brerr.readLine(), SimpleLogger.LogLevel.Level.ERROR);
			while (br.ready())
				SimpleLogger.logger().log(":SOUFFLE-OUTPUT " + br.readLine(), SimpleLogger.LogLevel.Level.DEBUG);
		}

		try {
			p.waitFor();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
		souffleRunTimer.stop();
		SimpleLogger.logger().time("Run Souffle program: " + souffleRunTimer.getTime() + "ms");

		br.close();
		brerr.close();
	}

	public static String getCSVSeparatorEscaped() {
		if (getCSVSeparator() != '\t')
			return String.valueOf(getCSVSeparator());
		return "\\t";
	}

	public static CmdLineOpts parseCmdLineArgs(String[] args) {
		DefaultParser parser = new DefaultParser();
		CmdLineOpts ret = new CmdLineOpts();

		Option prettyPrint = Option.builder("p").longOpt("pretty-print").numberOfArgs(1)
			.desc("Pretty print the program in MetaDL (arg = metadl) or Souffle (arg = souffle) format.").build();
		Option eval = Option.builder("e").longOpt("eval").numberOfArgs(1)
			.desc("Evaluate the program using the internal (arg = metadl), parallel (arg = metadl-par), Souffle (arg = souffle), Hybrid (arg = hybrid) evaluator.").build();
		Option check = Option.builder("c").longOpt("check").hasArg(false)
			.desc("Check that the input represents a valid MetaDL program.").build();
		Option imp = Option.builder("i").longOpt("import").hasArg(false)
			.desc("Evaluate only the import statements and output the program representation relation(s).").build();
		Option gen = Option.builder("g").longOpt("gen-hybrid").hasArg(false)
			.desc("Generate a hybrid MetaDL-Souffle program.").build();

		OptionGroup actions = new OptionGroup().addOption(eval).addOption(prettyPrint).addOption(check)
			.addOption(imp).addOption(gen);

		Option factDir = Option.builder("F").longOpt("facts").numberOfArgs(1)
			.desc("Fact directory.").argName("DIR").build();
		Option outDir = Option.builder("D").longOpt("out").numberOfArgs(1)
			.desc("Output directory.").argName("DIR").build();
		Option genDir = Option.builder("G").longOpt("gen").numberOfArgs(1)
			.desc("Generated program dirctory.").argName("DIR").build();
		Option outFile = Option.builder("o").longOpt("output").numberOfArgs(1)
			.desc("Output file.").argName("FILE").build();
		Option libFile = Option.builder("l").longOpt("lib").numberOfArgs(1)
			.desc("Library file to use for hybrid evaluation.").argName("FILE").build();
		Option enableWarnings = Option.builder("w").longOpt("warn").hasArg(false)
			.desc("Print warnings.").build();


		Options options = new Options().addOptionGroup(actions).
			addOption(factDir).addOption(outDir).addOption(genDir).addOption(outFile).addOption(libFile).addOption(enableWarnings);

		try {
			CommandLine cmd = parser.parse(options, args);
			if (cmd.getArgs().length != 1) {
				System.err.println("Missing PROGRAM argument.");
				printHelp(options);
				throw new RuntimeException();
			} else {
				ret.setInputFile(cmd.getArgs()[0]);
			}

			if (cmd.hasOption("p")) {
				if (cmd.getOptionValue("p").equals("souffle")) {
					ret.setAction(Action.PRETTY_SOUFFLE);
				} else if (cmd.getOptionValue("p").equals("metadl")) {
					ret.setAction(Action.PRETTY_INTERNAL);
				} else if (cmd.getOptionValue("p").equals("types")) {
					ret.setAction(Action.PRETTY_TYPES);
				} else {
					System.err.println("Invalid argument to '--pretty-print' option");
					printHelp(options);
					throw new RuntimeException();
				}
			} else if (cmd.hasOption("e")) {
				if (cmd.getOptionValue("e").equals("souffle")) {
					ret.setAction(Action.EVAL_SOUFFLE);
				} else if (cmd.getOptionValue("e").equals("metadl")) {
					ret.setAction(Action.EVAL_INTERNAL);
				} else if (cmd.getOptionValue("e").equals("metadl-par")) {
					ret.setAction(Action.EVAL_INTERNAL_PARALLEL);
				} else if (cmd.getOptionValue("e").equals("hybrid")) {
					ret.setAction(Action.EVAL_HYBRID);
				} else {
					System.err.println("Invalid argument to '--eval' option");
					printHelp(options);
					throw new RuntimeException();
				}
			} else if (cmd.hasOption("c")) {
				ret.setAction(Action.CHECK);
			} else if (cmd.hasOption("i")) {
				ret.setAction(Action.EVAL_IMPORT);
			} else if (cmd.hasOption("g")) {
				ret.setAction(Action.GEN_HYBRID);
			}

			ret.setFactsDir(cmd.getOptionValue("F", "."));
			ret.setOutputDir(cmd.getOptionValue("D", "."));
			ret.setGeneratedDir(cmd.getOptionValue("G", "."));
			ret.setOutputFile(cmd.getOptionValue("o",
												 ret.getOutputDir() + "/" +
												 FileUtil.changeExtension(FileUtil.fileName(ret.getInputFile()), ".dl")));
			ret.setLibFile(cmd.getOptionValue("l", "libSwigInterface.so"));
			ret.setWarningsEnabled(cmd.hasOption("w"));
		} catch (ParseException e) {
			printHelp(options);
			throw new RuntimeException(e);
		}

		return ret;
	}

	public static Program run(CmdLineOpts opts) {
		try {
			Program prog = parseAndCheckProgram(opts);

			switch (opts.getAction()) {
			case EVAL_INTERNAL:
				prog.eval(opts);
				break;
			case EVAL_INTERNAL_PARALLEL:
				prog.evalParallel(opts);
				break;
			case EVAL_SOUFFLE:
				prog.evalEDB(prog.evalCtx(), opts);
				prog.generateObjectProgramRelations(opts);
				evalSouffleProgram(prog, opts);
				break;
			case PRETTY_SOUFFLE:
				prog.evalEDB(prog.evalCtx(), opts);
				prettyPrintSouffle(prog, opts.getOutputDir() + "/" + opts.getOutputFile());
				break;
			case PRETTY_INTERNAL:
				StandardPrettyPrinter<Program> spp = new StandardPrettyPrinter<>(new PrintStream(System.out));
				spp.prettyPrint(prog);
				break;
			case PRETTY_TYPES:
				prog.dumpTypes(System.out);
				break;
			case EVAL_IMPORT:
				prog.generateObjectProgramRelations(opts);
				break;
			case CHECK:
				prog.dumpStrata();
				prog.clauseDependencyGraph().dump();
				break;
			case GEN_HYBRID:
				prog.evalEDB(prog.evalCtx(), opts);
				generateSouffleSWIGProgram(prog, opts);
				break;
			case EVAL_HYBRID:
				SWIGUtil.evalHybridProgram(prog, opts);
				break;
			}

			return prog;
		} catch (IOException e) {
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

		CmdLineOpts opts = parseCmdLineArgs(args);
		run(opts);

		totalTime.stop();
		SimpleLogger.logger().time("Total: " + totalTime.getTime() + "ms ");
	}
}
