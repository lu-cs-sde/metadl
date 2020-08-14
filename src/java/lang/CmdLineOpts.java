package lang;

public class CmdLineOpts {
	private String outputDir;
	private String factsDir;
	private static CmdLineOpts instance = new CmdLineOpts();

	public static void setOutputDir(String str) {
		instance.outputDir = str;
	}

	public static String getOutputDir() {
		return instance.outputDir;
	}

	public static String getFactsDir() {
		return instance.factsDir;
	}

	public static void setFactsDir(String factsDir) {
		instance.factsDir = factsDir;
	}
}
