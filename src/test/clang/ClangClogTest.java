package clang;

import static org.junit.jupiter.api.Assertions.fail;

import java.util.List;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import clang.swig.ClangClog;
import clang.swig.ClangClog.Loc;
import clang.swig.ClangClogBuilder;
import clang.swig.VectorLong;
import clang.swig.VectorString;
import clang.swig.VectorVectorLong;



public class ClangClogTest {
	@BeforeAll
	public static void prepare() {
		System.loadLibrary("clangClogSWIG");
	}

	@Test public void test1() {
		List<String> tests = List.of(
									 "tests/clang/evaluation/src/loop_depth/call_in_loop.c",
									 "tests/clang/evaluation/src/loop_depth/duff.c",
									 "tests/clang/evaluation/src/loop_depth/loops.c"
									 );
		ClangClogBuilder builder = new ClangClogBuilder(new VectorString(tests));

		ClangClog clog = builder.build();
	    if (!clog.init()) {
	    	fail("Error initializing clang-clog");
		}

		long declMatcherHandle = clog.registerMatcher("decl().bind(\"d\")", true);

		assert(declMatcherHandle >= 0);

		long forStmtHandle = clog.registerMatcher("forStmt().bind(\"f\")", false);

		assert(forStmtHandle >= 0);

		clog.runGlobalMatchers();

		VectorVectorLong declResult = clog.matchFromRoot((int)declMatcherHandle);

		for (VectorLong row : declResult) {
			for (long e :  row) {
				Loc srcLoc = clog.srcLocation((int)e);
				System.out.println(e + " : " + srcLoc.getFilename() + ": " + srcLoc.getStartLine() + ", " + srcLoc.getStartCol() + " - " +
								 srcLoc.getEndLine() + ", " + srcLoc.getEndCol());
				// VectorVectorLong forStmtResult = clog.matchFromNode((int)forStmtHandle, (int)e);

				// for (VectorLong row1 : forStmtResult) {
				// 	for (long e1 : row1) {
				// 		System.out.print(e + " : " + e1 + " ");
				// 	}
			}

		}
	}
}
