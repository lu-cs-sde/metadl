package lang.evaluation;

import java.util.Deque;

import lang.ast.Program;
import lang.ast.config.Description;
import lang.io.SimpleLogger;
import lang.relation.Stratification;
import lang.relation.Stratification.Stratum;

public class TopDownStratifiedRecursive extends Evaluation {
	@Override
	public void evaluate(Program program, Description descr) {
		Deque<Stratum> order = Stratification.stratification(program);
		
		while(!order.isEmpty()) {
			Stratum s = order.poll();
			SimpleLogger.logger().log("S: " + s, SimpleLogger.LogLevel.Level.DEBUG);
		}
		order = Stratification.stratification(program);
		System.out.println(order);
	}
}
