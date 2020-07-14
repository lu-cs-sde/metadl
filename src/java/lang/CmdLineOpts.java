package lang;

public class CmdLineOpts {
	private String outputDir;
	private String factsDir;
	private String outputFile;
	private String inputFile;
	private Action action;


	public enum Action {
		EVAL_SOUFFLE,
		EVAL_INTERNAL,
		EVAL_IMPORT,
		PRETTY_SOUFFLE,
		PRETTY_INTERNAL,
		CHECK
	}

	public void setOutputDir(String str) {
		this.outputDir = str;
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