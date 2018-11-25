package lang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import beaver.Parser.Exception;
import lang.ast.Program;
import lang.io.FileUtil;
import lang.io.SimpleLogger;

/**
 * Dumps the parsed Abstract Syntax Tree of a Calc program.
 */
public class Compiler {
	/**
	 * Entry point
	 * 
	 * @param args
	 */
	public static Object DrAST_root_node; // Enable debugging with DrAST

	public static void main(String[] args) {
		try {
			if (args.length == 0) {
				printUsage();
				System.exit(1);
				return;
			}
			Program program = (Program) FileUtil.parse(new File(args[0]));
			DrAST_root_node = program; // Enable debugging with DrAST
		} catch (FileNotFoundException e) {
			SimpleLogger.logger().log("File not found!", SimpleLogger.LogLevel.Level.ERROR);
			System.exit(1);
		} catch (IOException e) {
			e.printStackTrace(System.err);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static void printUsage() {
		SimpleLogger.logger().log("internal::topdownbasic|external::souffle [-OUT <OUT_DIR>] [-FACTS <FACT_DIR>]", SimpleLogger.LogLevel.Level.ERROR);
	}
}
