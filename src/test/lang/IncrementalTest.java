package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.collections4.SetUtils;
import org.apache.commons.io.FileUtils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

import eval.EvaluationContext;
import eval.Relation2;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.Program;
import lang.io.CSVUtil;
import lang.io.SQLUtil;
import lang.relation.RelationWrapper;

public class IncrementalTest {
	static File OUTPUT_DIR = new File("./tests/output/incremental");
	static File PROGRAM_DIR = new File("./tests/evaluation/metadl-java/");
	static File ANALYZED_DIR = new File("./tests/evaluation/metadl-java/src");
	static File EXPECTED_DIR = new File("./tests/evaluation/metadl-java/expected");

	@BeforeAll
	public static void init() throws IOException {
		if (OUTPUT_DIR.exists()) {
			FileUtils.deleteDirectory(OUTPUT_DIR);
		}
		OUTPUT_DIR.mkdirs();
	}

	private static Set<String> computeOutputRelations(EvaluationContext ctx, Program p) {
		Set<String> res = new TreeSet<>();
		// Find out which relations are output relations in the original program
		// Clear the contents of the relations (we use integers to store strings, but
		// their mapping may have been created in a different context, so we have
		// to clear everything before re-running.
		ctx.resetRelations();
		FormalPredicate OUTPUT = p.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);
		OUTPUT.eval(ctx);

		RelationWrapper OUTPUTTuples = new RelationWrapper(ctx, ctx.getRelation(OUTPUT), OUTPUT.type());

		// Read all the outputs from the program database
		for (RelationWrapper.TupleWrapper t : OUTPUTTuples.tuples()) {
			String predName = t.getAsString(0);
			res.add(predName);
		}

		return res;
	}

	private static void runProgramAndExtractResults(String name, boolean useSouffle) throws SQLException, IOException {
		String src = new File(PROGRAM_DIR, name + ".mdl").getPath();

		File output = new File(OUTPUT_DIR, name);
		output.mkdir();

		// Build the command-line options
		CmdLineOpts opts = new CmdLineOpts();
		opts.setAction(useSouffle ? CmdLineOpts.Action.INCREMENTAL_INIT : CmdLineOpts.Action.INCREMENTAL_INIT_INTERNAL);
		opts.setOutputDir(output.getPath());
		opts.setCacheDir(output.getPath());
		opts.setFactsDir(".");
		opts.setInputFile(src);

		// Run the split program
		EvaluationContext ctx = new EvaluationContext();
		Program p = Compiler.run(ctx, opts);

		Set<String> outputRelations = computeOutputRelations(ctx, p);
		for (String predName : outputRelations) {
			FormalPredicate pred = p.formalPredicateMap().get(predName);

			Relation2 rel = new Relation2(pred.type().arity());
			CSVUtil.readRelation(ctx, pred.type(), rel, Paths.get(output.getPath(), predName + ".csv").toString());
			// SQLUtil.readRelation(ctx, pred.type(), rel, Paths.get(output.getPath(), "srcs", "program.db").toString(), predName);
			RelationWrapper relW = new RelationWrapper(ctx, rel, pred.type());

			Relation2 expectedRel = new Relation2(pred.type().arity());
			CSVUtil.readRelation(ctx, pred.type(), expectedRel, Paths.get(EXPECTED_DIR.getPath(), name, predName + ".csv").toString());
			RelationWrapper expectedRelW = new RelationWrapper(ctx, expectedRel, pred.type());

			if (!expectedRelW.tuples().equals(relW.tuples())) {
				System.err.println("Missing in EXPECTED:");
				System.err.println(SetUtils.difference(relW.tuples(), expectedRelW.tuples()));
				System.err.println("Missing in ACTUAL:");
				System.err.println(SetUtils.difference(expectedRelW.tuples(), relW.tuples()));
			}
			assertEquals(expectedRelW.tuples(), relW.tuples(), "Relation " + predName + " differs.");
		}
	}

	static Stream<String> initialRunTestsSouffle() {
		String[] tests = {"reference-equality"};
		return Arrays.stream(tests);
	}

	@DisplayName("Test the initial run of the incremental evaluation driver using Souffle.")
	@ParameterizedTest
	@Disabled
	// @MethodSource("initialRunTestsSouffle")
	@MethodSource("initialRunTestsInternal")
	public void testInitialRunSouffle(String name) throws Exception {
		runProgramAndExtractResults(name, true);
	}

	static Stream<String> initialRunTestsInternal() {
		// Exclude the java7 test since it contains a 0 arity predicate and the
		// cache database does not handle 0-arity predicates
		return JavaDLEvaluationTest.metadlJavaTests().filter(t -> !t.equals("java7"));
	}
}
