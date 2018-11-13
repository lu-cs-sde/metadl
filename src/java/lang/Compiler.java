package lang;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;

import beaver.Parser.Exception;
import lang.ast.LangParser;
import lang.ast.LangScanner;
import lang.ast.Program;

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
			if (args.length != 1) {
				System.err.println("You must specify a source file on the command line!");
				printUsage();
				System.exit(1);
				return;
			}

			String filename = args[0];
			LangScanner scanner = new LangScanner(new FileReader(filename));
			LangParser parser = new LangParser();
			Program program = (Program) parser.parse(scanner);
			DrAST_root_node = program; // Enable debugging with DrAST

			HashSet<String> serrs = program.semanticErrors();

			if (!serrs.isEmpty()) {
				System.out.println("Compilation failed with the following error messages: ");
				serrs.forEach(err -> System.out.println(err));
				System.exit(0);
			}
			Evaluation eval = new BacktrackingEvaluation();
			eval.evaluate(program);

            System.out.println(program.prettyPrint());
            program.soufflePrint("souffle_ex.dl");



			// StringBuilder sb = new StringBuilder();
			// program.collectMetaInfo(sb);
			// System.out.println(sb.toString());

			// System.out.println(program.dumpTree());

			/* for(ErrorMessage m : program.errors()) */
			/* System.out.println(m); */
			/*  */
			/* if(program.errors().size() == 0) */
			/* program.genCode(System.out); */

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
		System.err.println("Usage: DumpTree FILE");
		System.err.println("  where FILE is the file to be parsed");
	}
}
