package lang;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.HashSet;

import beaver.Parser.Exception;
import lang.ast.LangParser;
import lang.ast.LangScanner;
import lang.ast.Program;
import lang.ast.config.ConfigParser;
import lang.ast.config.ConfigScanner;
import lang.ast.config.Description;
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
			
			ConfigScanner configScanner = new ConfigScanner(new StringReader(sb.toString()));
			ConfigParser configParser   = new ConfigParser();
			Description descr = (Description) configParser.parse(configScanner);
			
			SimpleLogger.logger().log("IN: " + descr.inputFile()).log("OUT: " + descr.outputDir()).log("FACTS: " + descr.factsDir());

			LangScanner scanner = new LangScanner(new FileReader(descr.getInput().getPath()));
			LangParser parser = new LangParser();
			Program program = (Program) parser.parse(scanner);
			DrAST_root_node = program; // Enable debugging with DrAST
			
			HashSet<String> serrs = program.semanticErrors();
			if (!serrs.isEmpty()) {
				SimpleLogger.logger().log("Compilation failed with the following error messages: ", SimpleLogger.LogLevel.Level.ERROR);
				serrs.forEach(err -> SimpleLogger.logger().log(err));
				System.exit(0);
			}
			
			descr.evaluationMethod().evaluate(program);

		} catch (FileNotFoundException e) {
			System.out.println("File not found!");
			System.exit(1);
		} catch (IOException e) {
			e.printStackTrace(System.err);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private static void printUsage() {
		SimpleLogger.logger().log("internal|external.souffle -F <INPUT-FILE>", SimpleLogger.LogLevel.Level.ERROR);
	}
}
