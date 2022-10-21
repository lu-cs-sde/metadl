package clang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import org.junit.jupiter.api.Test;
import java.io.IOException;

public class ASTImporterTest {
	@Test
	public void test1() throws IOException {
		new ASTImporter().importAST("tests/clang/test1.c", null);
	}
}
