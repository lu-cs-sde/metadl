package swig;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.tuple.Pair;

import lang.CmdLineOpts;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.TupleInserter;

public class SWIGUtil {
	public static Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> loadSWIGProgram(Program prog, CmdLineOpts opts) {
		System.load(opts.getLibFile());

		String progName = FileUtil.fileNameNoExtension(opts.getInputFile());
		System.out.println(progName);
		SWIGSouffleProgram swigProg = SwigInterface.newInstance(progName.replace("-", "_"));
		Map<FormalPredicate, TupleInserter> fpToSoufflePredMap = new HashMap<>();

		for (FormalPredicate pred : prog.getFormalPredicates()) {
			String name = pred.getPRED_ID();
			SWIGSouffleRelation swigRel = swigProg.getRelation(name);
			if (swigRel == null) {
				SimpleLogger.logger().log("Relation '" + name + "'is not present in the Souffle program.");
				continue;
			}

			fpToSoufflePredMap.put(pred, new SWIGSouffleRelationAdapter(swigRel));
		}

		return Pair.of(swigProg, fpToSoufflePredMap);


	}

	public static void runSWIGProgram(SWIGSouffleProgram swigProg, CmdLineOpts opts) {
		swigProg.run();
		swigProg.printAll(opts.getOutputDir());
		swigProg.finalize();
	}

	public static void evalHybridProgram(Program prog, CmdLineOpts opts) throws IOException {
		Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> progInfoPair = loadSWIGProgram(prog, opts);
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), progInfoPair.getRight());
		runSWIGProgram(progInfoPair.getLeft(), opts);
	}
}
