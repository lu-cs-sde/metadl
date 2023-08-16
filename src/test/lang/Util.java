package lang;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Collectors;

import org.apache.commons.collections4.SetUtils;

import clang.ClangEvaluationContext;
import eval.EvaluationContext;
import eval.Relation2;
import lang.ast.FormalPredicate;
import lang.ast.GlobalNames;
import lang.ast.Program;
import lang.io.CSVUtil;
import lang.io.FileUtil;
import lang.io.SimpleLogger;
import lang.relation.RelationWrapper;


/** {@link Util} methods for running tests. */
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
            if(!expected.exists()) fail("");
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

  public static void printSimmetricDifference(RelationWrapper expected, RelationWrapper actual) {
    if (!expected.tuples().equals(actual.tuples())) {
      System.err.println("Present in ACTUAL, but missing in EXPECTED:");
      System.err.println(SetUtils.difference(actual.tuples(), expected.tuples()));
      System.err.println("Present in EXPECTED, but missing in ACTUAL:");
      System.err.println(SetUtils.difference(expected.tuples(), actual.tuples()));
    }
  }

  public static String[] parseTestDescription(String desc) {
    String[] split = desc.split(":");

    if (split.length != 2) {
      fail("Could not parse test description " + desc + ".");
      return null;
    } else {
      return split;
    }
  }


  public static Map<String, RelationWrapper> doSingleEvaluation(CmdLineOpts d1) throws Exception {
    System.out.println(d1.getInputFile());

    EvaluationContext ctx = d1.getLang() == CmdLineOpts.Lang.C4 ?
      new ClangEvaluationContext(d1.getSrcs().keySet(), Collections.emptyList()) :
      new EvaluationContext();
    Program program1 = Compiler.run(ctx, d1);
    FormalPredicate fpOut1 = program1.formalPredicateMap().get(GlobalNames.OUTPUT_NAME);

    if (fpOut1 == null)
      fail();

    // force the evaluation of the output predicate
    fpOut1.eval(ctx);

    HashMap<String, RelationWrapper> nameToRel = new HashMap<>();


    RelationWrapper outputTuples = new RelationWrapper(ctx, ctx.getRelation(fpOut1), fpOut1.type());

    for (RelationWrapper.TupleWrapper t : outputTuples.tuples()) {
      String pred = t.getAsString(0);
      FormalPredicate fp = program1.formalPredicateMap().get(pred);

      File in1 = new File(d1.getOutputDir() + "/" + fp.predicateName() + ".csv");

      Relation2 r1 = new Relation2(fp.realArity(), fp.getPRED_ID());
      CSVUtil.readRelation(ctx, fp.type(), r1, d1.getOutputDir() + "/" + fp.predicateName() + ".csv");

      SimpleLogger.logger().log("Read relation from: " + in1.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
      nameToRel.put(fp.predicateName(), new RelationWrapper(ctx, r1, fp.type()));
    }
    return nameToRel;
  }

  public static void doEvaluationTest(CmdLineOpts d1, CmdLineOpts d2) throws Exception {
    Map<String, RelationWrapper> rels1 = doSingleEvaluation(d1);
    Map<String, RelationWrapper> rels2 = doSingleEvaluation(d2);

    for (Map.Entry<String, RelationWrapper> sr : rels1.entrySet()) {
      RelationWrapper rel1 = sr.getValue();
      RelationWrapper rel2 = rels2.get(sr.getKey());
      assertTrue(rel2 != null);
      assertEquals(rel1.tuples(), rel2.tuples(), "Relation " + sr.getKey() + " differs.");
    }

    // so far we proved rels1 is include in rels2, check their sizes are equal
    if (rels1.size() != rels2.size()) {
      System.out.println("Rels1: " + rels1.toString());
      System.out.println("Rels2: " + rels2.toString());
    }
    assertEquals(rels1.size(), rels2.size());
  }


  public static void singleEvaluationTest(String outputDir, String factDir,
                      List<File>  analyzedFiles,
                      String srcDir, String expectedDir,
                      String fileName, String fileExt,
                      String lang,
                      String cmd) throws Exception {

    String sources = analyzedFiles.stream().map(f -> f.getPath() + ",A").collect(Collectors.joining(":"));

    String desc = cmd
      + " -L " + lang
      + " -D " + outputDir
      + " -F " + factDir
      + (sources.isEmpty() ? " " : " -S ") + sources
      + " " + srcDir + "/"
      + fileName + fileExt;

    System.out.println("RUN: java -jar compiler.jar " + desc);

    CmdLineOpts d1 = FileUtil.parseDescription(desc);

    Map<String, RelationWrapper> outRelations = doSingleEvaluation(d1);
    for (Map.Entry<String, RelationWrapper> rel : outRelations.entrySet()) {
      String expectedF = expectedDir + "/" + fileName + "/" + rel.getKey() + ".csv";
      Relation2 expectedRel = new Relation2(rel.getValue().getRelation().arity(),
                          rel.getValue().getRelation().getName());

      CSVUtil.readRelation(rel.getValue().getContext(),
                 rel.getValue().type(),
                 expectedRel,
                 expectedF);

      RelationWrapper expectedRelWrapper = new RelationWrapper(rel.getValue().getContext(),
                                   expectedRel,
                                   rel.getValue().type());

      Util.printSimmetricDifference(expectedRelWrapper, rel.getValue());

      String msg = "Mismatch in test " + fileName + ", relation " + rel.getKey();
      assertEquals(expectedRel, rel.getValue().getRelation(), msg);
    }
  }

}
