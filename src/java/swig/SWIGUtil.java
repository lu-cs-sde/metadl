package swig;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.sql.SQLException;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.commons.lang3.tuple.Pair;

import lang.CmdLineOpts;
import lang.ast.HFPProgram;
import lang.ast.FormalPredicate;
import lang.ast.Program;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.TupleInserter;
import static prof.Profile.profile;

public class SWIGUtil {
	public static Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> loadSWIGProgram(Iterable<FormalPredicate> prog_predicates, CmdLineOpts opts) {
		File libFile = new File(opts.getLibFile());
		String progName = FileUtil.fileNameNoExtension(opts.getInputFile());
		return loadSWIGProgram(prog_predicates, libFile, progName);
	}

	public static Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> loadSWIGProgram(Iterable<FormalPredicate> prog_predicates, File libFile, String progName) {
		System.load(libFile.getPath());

		System.out.println(progName);
		SWIGSouffleProgram swigProg = SwigInterface.newInstance(progName.replace("-", "_"));
		Map<FormalPredicate, TupleInserter> fpToSoufflePredMap = new HashMap<>();

		for (FormalPredicate pred : prog_predicates) {
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

	public static Pair<SWIGSouffleProgram, Map<String, TupleInserter>> loadSWIGProgramStringLinkage(Iterable<String> prog_predicates, CmdLineOpts opts) {
		File libFile = new File(opts.getLibFile());
		String progName = FileUtil.fileNameNoExtension(opts.getInputFile());
		return loadSWIGProgramStringLinkage(prog_predicates, libFile, progName);
	}

	public static Pair<SWIGSouffleProgram, Map<String, TupleInserter>>
	loadSWIGProgramStringLinkage(Iterable<String> prog_predicates, File libFile, String progName) {
		System.load(libFile.getPath());

		System.out.println(progName);
		SWIGSouffleProgram swigProg = SwigInterface.newInstance(progName.replace("-", "_"));
		Map<String, TupleInserter> fpToSoufflePredMap = new HashMap<>();

		for (String name : prog_predicates) {
			SWIGSouffleRelation swigRel = swigProg.getRelation(name);
			fpToSoufflePredMap.put(name, new SWIGSouffleRelationAdapter(swigRel, name));
		}

		return Pair.of(swigProg, fpToSoufflePredMap);

	}

	public static void runSWIGProgram(SWIGSouffleProgram swigProg, CmdLineOpts opts) {
		int hwThreads = Runtime.getRuntime().availableProcessors();
		StopWatch timer = StopWatch.createStarted();
		swigProg.setNumThreads(Math.max(1, hwThreads / 2));
		profile().startTimer("hybrid_program", "run");
		swigProg.run();
		profile().stopTimer("hybrid_program", "run");
		profile().startTimer("hybrid_program", "print");
		swigProg.printAll(opts.getOutputDir(), "");
		profile().stopTimer("hybrid_program", "print");
		swigProg.finalize();
		timer.stop();
		SimpleLogger.logger().time("Run SWIG program: " + timer.getTime() + "ms");
	}

	public static void evalHybridProgram(Program prog, CmdLineOpts opts) throws IOException, SQLException {
		Pair<SWIGSouffleProgram, Map<FormalPredicate, TupleInserter>> progInfoPair = loadSWIGProgram(prog.getFormalPredicates(), opts);
		prog.evalEDB(prog.evalCtx(), opts);
		prog.evalIMPORT(prog.evalCtx(), opts);
		prog.evalAnalyzeBlocks(prog.evalCtx(), opts, progInfoPair.getRight());
		runSWIGProgram(progInfoPair.getLeft(), opts);
	}

	public static void evalHFPProgram(HFPProgram fprog, CmdLineOpts opts) throws IOException, SQLException {
		Pair<SWIGSouffleProgram, Map<String, TupleInserter>> progInfoPair = loadSWIGProgramStringLinkage(fprog.getFormalPredicates(), opts);
		// prog.evalEDB(prog.evalCtx(), opts);
		// prog.evalIMPORT(prog.evalCtx(), opts);
		// prog.evalAnalyzeBlocks(prog.evalCtx(), opts, progInfoPair.getRight());
		lang.Compiler.perr("evaHFPProgram starting");
		fprog.run(progInfoPair.getRight(), opts);
		lang.Compiler.perr("SWIG program executing");
		runSWIGProgram(progInfoPair.getLeft(), opts);
		lang.Compiler.perr("Completed");
	}
}
