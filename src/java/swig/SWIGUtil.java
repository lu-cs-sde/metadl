package swig;

import java.io.IOException;

import lang.CmdLineOpts;
import lang.ast.FormalPredicate;
import lang.ast.IntegerType;
import lang.ast.Program;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.RelationWrapper;
import lang.relation.RelationWrapper.TupleWrapper;

public class SWIGUtil {
	public static void runSWIGProgram(Program prog, CmdLineOpts opts) throws IOException {
		System.loadLibrary("SwigInterface");

		String progName = FileUtil.fileNameNoExtension(opts.getInputFile());
		System.out.println(progName);
		SWIGSouffleProgram swigProg = SwigInterface.newInstance(progName.replace("-", "_"));

		for (FormalPredicate pred : prog.getFormalPredicates()) {
			String name = pred.getPRED_ID();
			SWIGSouffleRelation swigRel = swigProg.getRelation(name);
			if (swigRel == null) {
				SimpleLogger.logger().log("Relation '" + name + "'is not present in the Souffle program.");
				continue;
			}
			RelationWrapper rel = new RelationWrapper(prog.evalCtx(), pred.relation2(), pred.type());

			for (TupleWrapper tpl : rel.tuples()) {
				SWIGSouffleTuple swigTpl = swigRel.makeTuple();

				for (int i = 0; i < rel.type().arity(); ++i) {
					if (rel.type().get(i) == IntegerType.get()) {
						// TODO: this should be long instead of int
						swigTpl.add(tpl.getAsLong(i));
					} else {
						swigTpl.add(tpl.getAsString(i));
					}
				}

				swigRel.add(swigTpl);
			}
		}

		swigProg.run();
		swigProg.printAll(opts.getOutputDir());
		swigProg.finalize();
	}
}
