package lang.evaluation;

import lang.ast.Program;

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