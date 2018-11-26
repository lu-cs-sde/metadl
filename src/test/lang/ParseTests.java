package lang;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;


public class ParseTests {
	/** Directory where the test input files are stored. */
	private static final File TEST_DIRECTORY_VALID = new File("tests/parser/valid");
	private static final File TEST_DIRECTORY_INVALID = new File("tests/parser/invalid");
    private static final String[] VALID_TESTS = {
         "parseTest_1.in"
        ,"parseTest_3.in"
        ,"parseTest_4.in"
        ,"parseTest_5.in"
        ,"parseTest_6.in"
        ,"parseTest_7.in"
        ,"parseTest_8.in"
        ,"parseTest_9.in"
        ,"parseTest_10.in"
        ,"parseTest_11.in"
        ,"parseTest_12.in"
        ,"parseTest_13.in"
    };
    private static final String[] INVALID_TESTS = {
         "parseTest_1.in"
        ,"parseTest_2.in"
        ,"parseTest_3.in"
        ,"parseTest_4.in"
        ,"parseTest_5.in"
        ,"parseTest_6.in"
        ,"parseTest_7.in"
        ,"parseTest_8.in"
        ,"parseTest_9.in"
        ,"parseTest_10.in"
        ,"parseTest_11.in"
        ,"parseTest_12.in"
        ,"parseTest_13.in"
        ,"parseTest_14.in"
        ,"parseTest_15.in"
    };
    
    private void doTest_Valid(String testName) {
    	System.out.println(TEST_DIRECTORY_VALID + "/" + testName);
    	Util.testValidSyntax(TEST_DIRECTORY_VALID, testName);
    }
    
	@TestFactory public Collection<DynamicTest> ParseTests_Valid() {
        Collection<DynamicTest> dynamicTests = new ArrayList<>();
        Arrays.asList(VALID_TESTS).forEach(fn -> dynamicTests.add(DynamicTest.dynamicTest(fn, () -> doTest_Valid(fn))));
        return dynamicTests;
	}

    private void doTest_Invalid(String testName) {
    	System.out.println(TEST_DIRECTORY_INVALID + "/" + testName);
    	Util.testSyntaxError(TEST_DIRECTORY_INVALID, testName);
    }
    
	@TestFactory public Collection<DynamicTest> ParseTests_InValid() {
        Collection<DynamicTest> dynamicTests = new ArrayList<>();
        Arrays.asList(INVALID_TESTS).forEach(fn -> dynamicTests.add(DynamicTest.dynamicTest(fn, () -> doTest_Invalid(fn))));
        return dynamicTests;
	}
}
