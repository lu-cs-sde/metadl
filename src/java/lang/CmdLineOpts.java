package lang;

import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.ParseException;

import lang.io.FileUtil;

import org.apache.commons.cli.HelpFormatter;

import java.sql.Connection;

import org.apache.commons.cli.CommandLine;


public class CmdLineOpts {
	private String outputDir;
	private String factsDir;
	private String cacheDir;
	private String outputFile;
	private String inputFile;
	private String libFile;
	private String profileFile;
	private Connection sqlDbConnection; // Internal option
	private Integer dbEntryTag = null; // Internal option
	private Action action = Action.EVAL_INTERNAL;
	private boolean warningsEnabled = false;

	public enum Action {
		EVAL_SOUFFLE,
		EVAL_INTERNAL,
		EVAL_INTERNAL_PARALLEL,
		EVAL_IMPORT,
		EVAL_HYBRID,
		PRETTY_SOUFFLE,
		PRETTY_INTERNAL,
		PRETTY_TYPES,
		INCREMENTAL_INIT,
		INCREMENTAL_UPDATE,
		INCREMENTAL_INIT_INTERNAL,
		GEN_HYBRID,
		CHECK
	, INCREMENTAL_UPDATE_INTERNAL}

	public void setOutputDir(String str) {
		this.outputDir = str;
	}

	public String getProfileFile() {
		return profileFile;
	}

	public void setProfileFile(String profileFile) {
		this.profileFile = profileFile;
	}

	public Integer getDbEntryTag() {
		return dbEntryTag;
	}

	public void setDbEntryTag(Integer dbEntryTag) {
		this.dbEntryTag = dbEntryTag;
	}

	public Connection getSqlDbConnection() {
		return sqlDbConnection;
	}

	public void setSqlDbConnection(Connection conn) {
		this.sqlDbConnection = conn;
	}

	public boolean isWarningsEnabled() {
		return warningsEnabled;
	}

	public void setWarningsEnabled(boolean warningsEnabled) {
		this.warningsEnabled = warningsEnabled;
	}

	public String getLibFile() {
		return libFile;
	}

	public void setLibFile(String libFile) {
		this.libFile = libFile;
	}

	public Action getAction() {
		return this.action;
	}

	public void setAction(Action action) {
		this.action = action;
	}

	public String getInputFile() {
		return this.inputFile;
	}

	public void setInputFile(String inputFile) {
		this.inputFile = inputFile;
	}

	public String getOutputFile() {
		return this.outputFile;
	}

	public void setOutputFile(String outputFile) {
		this.outputFile = outputFile;
	}

	public String getOutputDir() {
		return this.outputDir;
	}

	public String getFactsDir() {
		return this.factsDir;
	}

	public void setFactsDir(String factsDir) {
		this.factsDir = factsDir;
	}

	public String getCacheDir() {
		return this.cacheDir;
	}

	public void setCacheDir(String cacheDir) {
		this.cacheDir = cacheDir;
	}

	@Override public String toString() {
		String s = "";
		s += "output dir : " + outputDir + "\n";
		s += "facts dir : " + factsDir + "\n";
		s += "output file : " + outputFile + "\n";
		s += "input file : " + inputFile + "\n";
		s += "action : " + action + "\n";
		return s;
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
		Option incremental = Option.builder("s").longOpt("incremental").numberOfArgs(1)
			.desc("Incrementally evaluate the program (init = initial run, update = subsequent runs).").build();

		OptionGroup actions = new OptionGroup().addOption(eval).addOption(prettyPrint).addOption(check)
			.addOption(imp).addOption(gen).addOption(incremental);

		Option factDir = Option.builder("F").longOpt("facts").numberOfArgs(1)
			.desc("Fact directory.").argName("DIR").build();
		Option outDir = Option.builder("D").longOpt("out").numberOfArgs(1)
			.desc("Output directory.").argName("DIR").build();
		Option genDir = Option.builder("C").longOpt("cache").numberOfArgs(1)
			.desc("Incremental evaluation cache.").argName("DIR").build();
		Option outFile = Option.builder("o").longOpt("output").numberOfArgs(1)
			.desc("Output file.").argName("FILE").build();
		Option libFile = Option.builder("l").longOpt("lib").numberOfArgs(1)
			.desc("Library file to use for hybrid evaluation.").argName("FILE").build();
		Option enableWarnings = Option.builder("w").longOpt("warn").hasArg(false)
			.desc("Print warnings.").build();
		Option profile = Option.builder("P").longOpt("profile").numberOfArgs(1)
			.desc("Enable profiling and dump the results in JSON format").argName("FILE").build();

		Options options = new Options().addOptionGroup(actions)
			.addOption(factDir).addOption(outDir).addOption(genDir).addOption(outFile)
			.addOption(libFile).addOption(enableWarnings).addOption(profile);

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
			} else if (cmd.hasOption("s")) {
				if (cmd.getOptionValue("s").equals("init")) {
					ret.setAction(Action.INCREMENTAL_INIT);
				} else if (cmd.getOptionValue("s").equals("update")) {
					ret.setAction(Action.INCREMENTAL_UPDATE);
				} else if (cmd.getOptionValue("s").equals("init-metadl")) {
					ret.setAction(Action.INCREMENTAL_INIT_INTERNAL);
				} else if (cmd.getOptionValue("s").equals("update-metadl")) {
					ret.setAction(Action.INCREMENTAL_UPDATE_INTERNAL);
				} else {
					System.err.println("Invalid argument to '--incremental' option");
					printHelp(options);
					throw new RuntimeException();
				}
			}

			if (cmd.hasOption("P")) {
				ret.setProfileFile(cmd.getOptionValue("P"));
			}

			ret.setFactsDir(cmd.getOptionValue("F", "."));
			ret.setOutputDir(cmd.getOptionValue("D", "."));
			ret.setCacheDir(cmd.getOptionValue("C", ret.getOutputDir()));
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

	public static void printHelp(Options options) {
		String header = "Compile and run a MetaDL program.\n\n";
		String footer = "\nPlease report issues at https://github.com/lu-cs-sde/metadl";

		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("metadl PROGRAM", header, options, footer, true);
	}
}
