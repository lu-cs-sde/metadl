package lang;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.lang3.time.StopWatch;

import lang.ast.config.Description;
import lang.io.FileUtil;
import lang.io.SimpleLogger;

/**
 * Dumps the parsed Abstract Syntax Tree of a Calc program.
 */
public class Compiler {
    public static Object DrAST_root_node; // Enable debugging with DrAST
	public static Description descr;

	public static char getCSVSeparator() {
		if (descr == null) {
			return ',';
		} else {
			return descr.CSVSeparator();
		}
	}

	public static String getCSVSeparatorEscaped() {
		if (getCSVSeparator() != '\t')
			return String.valueOf(getCSVSeparator());
		return "\\t";
	}

    public static void main(String[] args) {
		Logger log = Logger.getLogger("lang.io");
		log.setLevel(Level.FINEST);
		StopWatch totalTime = StopWatch.createStarted();

        if (args.length == 0) {
            printUsage();
            System.exit(0);
        }

        try {
            StringBuilder sb = new StringBuilder();
            for(int i = 0; i != args.length; ++i)
				sb.append(args[i]).append(" ");
            descr = FileUtil.parseDescription(sb.toString());
            descr.getTask().perform();
        } catch (Exception e) {
			e.printStackTrace();
		}

		totalTime.stop();
		SimpleLogger.logger().time("Total: " + totalTime.getTime() + "ms ");
    }

    private static void printUsage() {
        SimpleLogger.logger().log("(pretty|eval)::(bottomupnaive|souffle|import) [-OUT <OUT_DIR>] [-FACTS <FACT_DIR>] <INPUT_FILE>", SimpleLogger.LogLevel.Level.ERROR);
    }
}
