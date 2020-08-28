package lang;

public class CmdLineOpts {
	private String outputDir;
	private String factsDir;
	private String genDir;
	private String outputFile;
	private String inputFile;
	private String libFile;
	private Action action;

	public enum Action {
		EVAL_SOUFFLE,
		EVAL_INTERNAL,
		EVAL_INTERNAL_PARALLEL,
		EVAL_IMPORT,
		EVAL_HYBRID,
		PRETTY_SOUFFLE,
		PRETTY_INTERNAL,
		GEN_HYBRID,
		CHECK
	}

	public void setOutputDir(String str) {
		this.outputDir = str;
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

	public String getGeneratedDir() {
		return this.genDir;
	}

	public void setGeneratedDir(String genDir) {
		this.genDir = genDir;
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
}
