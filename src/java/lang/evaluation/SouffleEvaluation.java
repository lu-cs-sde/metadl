package lang.evaluation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;

import beaver.Parser.Exception;
import lang.ast.Program;
import lang.ast.SoufflePrettyPrinter;
import lang.ast.config.Description;
import lang.io.SimpleLogger;

public class SouffleEvaluation extends ExternalEvaluation {
	
	public void initialEvaluation(Program program, Description descr) throws IOException, Exception {
		// Description evalDescr = FileUtil.parseDescription("eval::bottomupnaive -OUT " + descr.factsDir() + " -FACTS " + descr.factsDir() + " " + descr.inputFile());
		// BottomUpNaiveIterative eval = new BottomUpNaiveIterative();
		// eval.evaluate(program, evalDescr);
	}

	@Override
	public void evaluate(Program program, Description descr) {
	/*     if (!descr.isSouffle()) { */
	/*         SimpleLogger.logger().log("Can not perform SouffleEvaluation on non-souffle program", */
	/*                 SimpleLogger.LogLevel.Level.ERROR); */
	/*         System.exit(0); */
	/*     } */
	/*     try { */
	/*          */
	/*          */
	/*          */
	/*         Souffle souffleDescr = (Souffle) descr.evalParams(); */
	/*         String soufflePath = descr.outputDir() + "/" + souffleDescr.outputFileName(); */
	/*         program.soufflePrint( */
	/*                 new SoufflePrettyPrinter<Program>(new PrintStream(new FileOutputStream(new File(soufflePath))))); */
    /*  */
	/*         String cmd = "souffle -D " + descr.outputDir() + " " + soufflePath + " -F " + descr.factsDir(); */
    /*  */
	/*         SimpleLogger.logger().log("Run souffle with: " + cmd, SimpleLogger.LogLevel.Level.DEBUG); */
    /*  */
	/*         Process p = Runtime.getRuntime().exec(cmd); */
	/*         p.waitFor(); */
    /*  */
	/*         BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream())); */
	/*         while (br.ready()) { */
	/*             SimpleLogger.logger().log(":SOUFFLE-OUTPUT " + br.readLine(), SimpleLogger.LogLevel.Level.DEBUG); */
	/*         } */
	/*         BufferedReader brerr = new BufferedReader(new InputStreamReader(p.getErrorStream())); */
	/*         while (brerr.ready()) { */
	/*             SimpleLogger.logger().log(":SOUFFLE-ERROR " + brerr.readLine(), SimpleLogger.LogLevel.Level.ERROR); */
	/*         } */
	/*         br.close(); */
	/*         brerr.close(); */
	/*     } catch (FileNotFoundException e) { */
	/*         e.printStackTrace(); */
	/*     } catch (IOException e) { */
	/*         e.printStackTrace(); */
	/*     } catch (InterruptedException e) { */
	/*         e.printStackTrace(); */
	/*     } */
    }
}
