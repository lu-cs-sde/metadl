package lang.evaluation;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import lang.ast.Program;
import lang.ast.SoufflePrettyPrinter;
import lang.ast.config.Description;

public class SouffleEvaluation extends ExternalEvaluation {
	
	@Override
    public void evaluate(Program program) {
//		try {
//			program.soufflePrint(new SoufflePrettyPrinter<Program>(new PrintStream(new FileOutputStream(new File(directory + "/" + fileName)))));
//			Process p = Runtime.getRuntime().exec("souffle -D " + directory + " " + fileName);
//			p.waitFor();
//		} catch (FileNotFoundException e) {
//			e.printStackTrace();
//		} catch (IOException e) {
//			e.printStackTrace();
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}
    }
}