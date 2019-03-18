package lang;

import java.util.logging.Level;
import java.util.logging.Logger;

import lang.ast.config.Description;
import lang.io.FileUtil;
import lang.io.SimpleLogger;

/**
 * Dumps the parsed Abstract Syntax Tree of a Calc program.
 */
public class Compiler {
    public static Object DrAST_root_node; // Enable debugging with DrAST
    public static void main(String[] args) {
		Logger log = Logger.getLogger("lang.io");
		log.setLevel(Level.FINEST);


        if (args.length == 0) {
            printUsage();
            System.exit(0);
        }

        try {
            StringBuilder sb = new StringBuilder();
            for(int i = 0; i != args.length; ++i) sb.append(args[i]).append(" ");
            Description descr = FileUtil.parseDescription(sb.toString());
            descr.getTask().perform();
        } catch (Exception e) {
			e.printStackTrace();
		}
    }

    private static void printUsage() {
        SimpleLogger.logger().log("(pretty|eval)::(bottomupnaive|souffle) [-OUT <OUT_DIR>] [-FACTS <FACT_DIR>] <INPUT_FILE>", SimpleLogger.LogLevel.Level.ERROR);
    }
}
