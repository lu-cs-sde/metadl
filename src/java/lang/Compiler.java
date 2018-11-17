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
			
			
			StringBuilder sb = new StringBuilder();
			for(int i = 0; i != args.length; ++i) {
				sb.append(args[i]);
				sb.append(" ");
			}
			System.out.println("Got: " + sb.toString());
			
			ConfigScanner configScanner = new ConfigScanner(new StringReader(sb.toString()));
			ConfigParser configParser   = new ConfigParser();
			Description descr = (Description) configParser.parse(configScanner);
			
			System.out.println("IN: " + descr.inputFile());
			System.out.println("OUT: " + descr.outputDir());
			System.out.println("FACTS: " + descr.factsDir());

			LangScanner scanner = new LangScanner(new FileReader(descr.getInput().getPath()));
			LangParser parser = new LangParser();
			Program program = (Program) parser.parse(scanner);
			DrAST_root_node = program; // Enable debugging with DrAST
			

			HashSet<String> serrs = program.semanticErrors();

			if (!serrs.isEmpty()) {
				System.out.println("Compilation failed with the following error messages: ");
				serrs.forEach(err -> System.out.println(err));
				System.exit(0);
			}
			// BacktrackingEvaluation eval = new BacktrackingEvaluation();
			// eval.evaluate(program);

            System.out.println(program.prettyPrint());
            program.soufflePrint("souffle_ex.dl");
            
//            program.getSuperPredicates().forEach(sp -> {
//            	eval.deriveAllFacts(sp, program);
//            	CSVUtil.dumpFileInto(sp, new File("output/" + sp.predicateName() + ".csv"));
//            });


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
		System.err.println("internal|external.souffle -F <INPUT-FILE>");
	}
}
