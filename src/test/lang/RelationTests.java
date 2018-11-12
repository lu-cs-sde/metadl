package lang;

import java.io.File;
import java.util.HashSet;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import lang.ast.Program;


public class RelationTests {
    /** Directory where the test input files are stored. */
    private static final File TEST_DIRECTORY_VALID = new File("tests/relations/valid");
    private static final File TEST_DIRECTORY_INVALID = new File("tests/relations/invalid");
    
   
    public void doTest(Program p, String filename, File dir) {
		StringBuilder sb = new StringBuilder();
        HashSet<String> serrs = p.semanticErrors();
        for(String serr : serrs) {
            sb.append(serr).append("\n");
        }
        
        if(serrs.size() == 0) {
        	p.collectInitialMetaInfo(sb);
        	sb.append("\n");
        }

		Util.compareOutput(sb.toString(), new File(dir, Util.changeExtension(filename, "_relationTests.out")),
				new File(dir, Util.changeExtension(filename, "_relationTests.expected")));
    }
    
    @DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Valid")
	@ValueSource(strings = {
            "relationsTest_1.in"
            ,"relationsTest_2.in"
            ,"relationsTest_3.in"
	})
	public void relationTestsValid(String filename) throws Exception {
		System.out.println("valid/"+filename);
		Program program = (Program) Util.parse(new File(TEST_DIRECTORY_VALID, filename));
		doTest(program, filename, TEST_DIRECTORY_VALID);
	}

    @DisplayName("Test Relation Data such as File Constants")
	@ParameterizedTest(name = "Relation Tests Invalid")
	@ValueSource(strings = {
            "relationsTest_1.in"
            ,"relationsTest_2.in"
            ,"relationsTest_3.in"
	})
	public void relationTestsInvalid(String filename) throws Exception {
		System.out.println("invalid/" + filename);
		Program program = (Program) Util.parse(new File(TEST_DIRECTORY_INVALID, filename));
		doTest(program, filename, TEST_DIRECTORY_INVALID);
	}

}
