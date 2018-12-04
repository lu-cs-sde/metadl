package lang.evaluation;

import java.util.HashSet;

import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.ast.config.Description;
import lang.io.FileUtil;
import lang.io.SimpleLogger;

public class TypeProgEvaluation extends InternalEvaluation {
    public void evaluate(Program program, Description descr) {
		HashSet<String> serrs = program.semanticErrors();
		if (!serrs.isEmpty()) {
			SimpleLogger.logger().log("Compilation failed with the following error messages: ", SimpleLogger.LogLevel.Level.ERROR);
			serrs.forEach(err -> SimpleLogger.logger().log(err));
			System.exit(0);
		}
		String typeProgSource = program.typePrint();
        System.out.println("------------------------------------------------");
        System.out.println("\t\t\t TYPESOURCE");
        System.out.println("------------------------------------------------");
        System.out.println(typeProgSource);
        System.out.println("------------------------------------------------");
        try {
            Program typeProg = FileUtil.parse(typeProgSource);
            BottomUpNaiveIterative eval = new BottomUpNaiveIterative();
            eval.evaluate(typeProg, null);

            System.out.println("------------------------------------------------");
            System.out.println("\t\t\t EVAL RESULT");
            System.out.println("------------------------------------------------");
            for(FormalPredicate fp : typeProg.getFormalPredicates()) {
                System.out.println(fp.predicateName() + ": " + fp.relation);
            }
            System.out.println("------------------------------------------------");
			program.typeCheck();
			HashSet<String> terrs = program.typeCheckErrors();
			if (!terrs.isEmpty()) {
				terrs.forEach(err -> SimpleLogger.logger().log(err));
				System.exit(0);
			}
            System.out.println("------------------------------------------------");
            System.out.println("\t\t\t TYPEINFERENCE RESULT");
            System.out.println("------------------------------------------------");
			for(FormalPredicate fp : program.getFormalPredicates()) {
                if(fp.literal().isInclusive())
                    System.out.println(fp.predicateName() + ": " + fp.derivedTypes());
			}
            System.out.println("------------------------------------------------");
        } catch(Exception e) {
        	e.printStackTrace();
        }
    }
}
