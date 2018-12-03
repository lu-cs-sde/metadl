package lang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashSet;

import beaver.Parser.Exception;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.config.Description;
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
			/**
			 * Collect program args and parse to collect problem description
			 */
			StringBuilder sb = new StringBuilder();
			for(int i = 0; i != args.length; ++i) {
				sb.append(args[i]);
				sb.append(" ");
			}
			
			Description descr = FileUtil.parseDescription(sb.toString());
			SimpleLogger.logger().log(descr.debugInfo());
			Program program = (Program) FileUtil.parse(new File(descr.getInput().getPath()));
			DrAST_root_node = program; // Enable debugging with DrAST
			
			HashSet<String> serrs = program.semanticErrors();
			if (!serrs.isEmpty()) {
				SimpleLogger.logger().log("Compilation failed with the following error messages: ", SimpleLogger.LogLevel.Level.ERROR);
				serrs.forEach(err -> SimpleLogger.logger().log(err));
				System.exit(0);
			}
			SimpleLogger.logger().log("PASS1TMAP: " + program.pass1TypeMap(), SimpleLogger.LogLevel.Level.DEBUG);
			
			program.typeCheck();
			HashSet<String> terrs = program.typeCheckErrors();
			if (!terrs.isEmpty()) {
				SimpleLogger.logger().log("Compilation failed with the following error messages: ", SimpleLogger.LogLevel.Level.ERROR);
				terrs.forEach(err -> SimpleLogger.logger().log(err));
				System.exit(0);
			}
			descr.evaluationMethod().evaluate(program, descr);
			
			for(FormalPredicate fp : program.getFormalPredicates()) {
				System.out.println("TYPEOF " + fp.predicateName() + ": " + fp.derivedTypes());
			}
			
		} catch (FileNotFoundException e) {
			SimpleLogger.logger().log("File not found!", SimpleLogger.LogLevel.Level.ERROR);
			System.exit(1);
		} catch (IOException e) {
			e.printStackTrace(System.err);
		} catch (Exception e) {
			e.printStackTrace();
		} catch (java.lang.Exception e) {
			e.printStackTrace();
		}
	}

	private static void printUsage() {
		SimpleLogger.logger().log("(pretty|eval)::(bottomupnaive|souffle) [-OUT <OUT_DIR>] [-FACTS <FACT_DIR>] <INPUT_FILE>", SimpleLogger.LogLevel.Level.ERROR);
	}
}
