package swig;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.sql.SQLException;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;

import lang.CmdLineOpts;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.java.obj.DatalogProjectionSink;
import lang.relation.TupleInserter;

public class SWIGUtil {
	public static Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> loadSWIGProgram(Program prog, CmdLineOpts opts) {
		File libFile = new File(opts.getLibFile());
		String progName = FileUtil.fileNameNoExtension(opts.getInputFile());
		return loadSWIGProgram(prog, libFile, progName);
	}

	public static Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> loadSWIGProgram(Program prog, File libFile, String progName) {
		System.load(libFile.getPath());

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

			fpToSoufflePredMap.put(pred, new SWIGSouffleRelationAdapter(swigRel, pred.getPRED_ID()));
		}

		return Pair.of(swigProg, fpToSoufflePredMap);

	}

	public static void runSWIGProgram(SWIGSouffleProgram swigProg, CmdLineOpts opts) {
		int hwThreads = Runtime.getRuntime().availableProcessors();
		StopWatch timer = StopWatch.createStarted();
		swigProg.setNumThreads(Math.max(1, hwThreads / 2));
		swigProg.run();
		swigProg.printAll(opts.getOutputDir(), "");
		swigProg.finalize();
		timer.stop();
		SimpleLogger.logger().time("Run SWIG program: " + timer.getTime() + "ms");
	}

	public static void evalHybridProgram(Program prog, CmdLineOpts opts) throws IOException, SQLException {
		Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> progInfoPair = loadSWIGProgram(prog, opts);
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), opts);
		prog.evalAnalyzeBlocks(prog.evalCtx(), opts, progInfoPair.getRight());
		runSWIGProgram(progInfoPair.getLeft(), opts);
	}
}
