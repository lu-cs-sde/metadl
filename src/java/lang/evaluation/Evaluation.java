package lang.evaluation;

import lang.ast.Program;
import lang.ast.config.Description;

public abstract class Evaluation {
    public abstract void evaluate(Program program, Description descr);
    
    
}
