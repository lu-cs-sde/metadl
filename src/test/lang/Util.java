package lang;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Scanner;

import lang.io.FileUtil;

/** Utility methods for running tests. */
public final class Util {
	private static String SYS_LINE_SEP = System.getProperty("line.separator");

	private Util() { }



	/**
	 * Check that the string matches the contents of the given file.
	 * Also writes the actual output to file.
	 * @param actual actual output
	 * @param out file where output should be written
	 * @param expected file containing expected output
	 */
	public static void compareOutput(String actual, File out, File expected) {
		try {
			Files.write(out.toPath(), actual.getBytes());
			assertEquals(readFileToString(expected),
				normalizeText(actual),"Output differs");
		} catch (IOException e) {
			fail("IOException occurred while comparing output: " + e.getMessage());
		}
	}

    public static void compareOutputFiles(File f1, File f2) {
		try {
			assertEquals(readFileToString(f1),
                         readFileToString(f2),
                        "Output differs");
		} catch (IOException e) {
			fail("IOException occurred while comparing output: " + e.getMessage());
		}
    }

	/**
	 * Reads an entire file to a string object.
	 * <p>If the file does not exist an empty string is returned.
	 * <p>The system dependent line separator char sequence is replaced by
	 * the newline character.
	 *
	 * @return normalized text from file
	 */
	private static String readFileToString(File file) throws FileNotFoundException {
		if (!file.isFile()) {
			return "";
		}

		Scanner scanner = new Scanner(file);
		scanner.useDelimiter("\\Z");
		String text = normalizeText(scanner.hasNext() ? scanner.next() : "");
		scanner.close();
		return text;
	}

	/** Trim whitespace and normalize newline characters. */
	private static String normalizeText(String text) {
		return text.replace(SYS_LINE_SEP, "\n").trim();
	}

	public static void testValidSyntax(File directory, String filename) {
		try {
			FileUtil.parse(new File(directory, filename));
		} catch (Exception e) {
			fail("Unexpected error while parsing '" + filename + "': "
					+ e.getMessage());
		}
	}

	public static void testSyntaxError(File directory, String filename) {
	  PrintStream prevErr = System.err;

		// Beaver reports syntax error on the standard error.
		// We discard these messages since a syntax error is expected.
		System.setErr(new PrintStream(new ByteArrayOutputStream()));
		try {
			FileUtil.parse(new File(directory, filename));

			fail("syntax is valid, expected syntax error");
		} catch (beaver.Parser.Exception | lang.ast.LangParser.SyntaxError e) {
			// Ok (expected syntax error)!
		} catch (Exception e) {
			fail("IO error while trying to parse '" + filename + "': "
					+ e.getMessage());
		} finally {
			// Restore the system error stream.
			System.setErr(prevErr);
		}
	}
	



	@SuppressWarnings("javadoc")
	public static Iterable<Object[]> getTestParameters(File testDirectory, String extension) {
		Collection<Object[]> tests = new LinkedList<Object[]>();
		if (!testDirectory.isDirectory()) {
			throw new Error("Could not find '" + testDirectory + "' directory!");
		}
		for (File f: testDirectory.listFiles()) {
			if (f.getName().endsWith(extension)) {
				tests.add(new Object[] {f.getName()});
			}
		}
		return tests;
	}
}
