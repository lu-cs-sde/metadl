package lang;

import static org.junit.jupiter.api.Assertions.assertTrue;
import java.io.File;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import lang.ast.Program;
import lang.io.FileUtil;

public class SemanticErrorTests {
	public void comparisonTest(String path) {
		File input = new File(path);
		File output = new File(FileUtil.changeExtension(path, ".out"));
		File expected = new File(FileUtil.changeExtension(path, ".expected"));

		try {
			Program program = (Program) FileUtil.parse(input);
			StringBuilder sb = new StringBuilder();
			for(String err : program.semanticErrors())
				sb.append(err).append("\n");
			Util.compareOutput(sb.toString(), output, expected);
		} catch(Exception e) {
			e.printStackTrace();
			assertTrue(false);
		}
	}

	@DisplayName("Compare Internal Evaluation to Souffle WithMeta")
	@ParameterizedTest(name = "Evaluation Tests Valid")
	@ValueSource(strings = { "semantic1.in", "semantic2.in", "semantic3.in",
							 "semantic4.in", "semantic5.in", "semantic6.in",
							 "semantic7.in", "semantic8.in", "semantic9.in",
							 "semantic10.in"
						   })
	public void semanticTests(String fileName) {
		String dir = "tests/semantic/";
		comparisonTest(dir + fileName);
	}
}
