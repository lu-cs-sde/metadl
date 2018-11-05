package lang;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;


public class ParseTests {
	/** Directory where the test input files are stored. */
	private static final File TEST_DIRECTORY = new File("tests/parser");

    private static final String[] VALID_TESTS = {
        "divExpr.in"
       ,"divExpr2.in"
       ,"divExpr2.in"
    };
    
    private void doTest(String testName) {
    	System.out.println(TEST_DIRECTORY + "/" + testName);
    	Util.testSyntaxError(TEST_DIRECTORY, testName);
    }
    
	@TestFactory public Collection<DynamicTest> ParseTests_Valid() {
        Collection<DynamicTest> dynamicTests = new ArrayList<>();
        Arrays.asList(VALID_TESTS).forEach(fn -> dynamicTests.add(DynamicTest.dynamicTest(fn, () -> doTest(fn))));
        return dynamicTests;
	}

	@Test public void ParseTests_errorNoStatement() {
		Util.testSyntaxError(TEST_DIRECTORY, "errorNoStatement.in");
	}
	
	@Test public void ParseTests_errorNoStatement2() {
		System.out.println("Running Test");
		Util.testSyntaxError(TEST_DIRECTORY, "divExpr2.in");
	}
}
