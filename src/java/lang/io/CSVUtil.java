package lang.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;

import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import com.opencsv.CSVWriter;

import lang.ast.FormalPredicate;
import lang.ast.IntConstant;
import lang.ast.PredicateRef;
import lang.ast.Program;
import lang.ast.StringConstant;
import lang.ast.Term;
import lang.ast.Variable;
import lang.relation.PseudoTuple;
import lang.relation.Relation;
import lang.Compiler;

public class CSVUtil {
	public static boolean isInteger(String s) {
		try {
			Integer.parseInt(s);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	public static Term parseCSV(Program program, String line) {
		if(isInteger(line)) {
			return new IntConstant(line);
		}
		if(!line.isEmpty() && line.charAt(0) == '\'') {
			if(program != null && program.formalPredicateMap().get(line.substring(1)) == null) {
				SimpleLogger.logger().log("Must only reference exisiting predicates: " + line, SimpleLogger.LogLevel.Level.ERROR);
			}

			/**
			 * Need to be careful here: The PredicateRef will NOT belong to the AST and so cannot use e.g. literal().
			 * For future development should really make this explicit through the type-system ...
			 */
			return new PredicateRef(line.substring(1));
		}
		return new StringConstant(line);
	}

	public static void readFileInto(Program program, Relation r, String path) {
		SimpleLogger.logger().log("Read file " + path, SimpleLogger.LogLevel.Level.DEBUG);

		CSVParser parser = new CSVParserBuilder().withSeparator(Compiler.getCSVSeparator()).build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(new File(path))).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				if (line.length != r.arity()) {
					SimpleLogger
							.logger().log(
									"Expected arity: " + r.arity() + " but got "
											+ line.length + "-sized tuple in .csv file",
									SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}
				PseudoTuple ps = new PseudoTuple(r.arity());
				for (int i = 0; i != line.length; ++i)
					ps.set(i, parseCSV(program, line[i]));
				r.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void readFileInto(Program program, FormalPredicate fp, String path) {
		SimpleLogger.logger().log("Begin Read file " + path + " into " + fp, SimpleLogger.LogLevel.Level.DEBUG);

		CSVParser parser = new CSVParserBuilder().withSeparator(Compiler.getCSVSeparator()).build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(new File(path))).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				if (line.length != fp.realArity()) {
					SimpleLogger
							.logger().log(
									"Predicate: " + fp.predicateName() + " has arity: " + fp.realArity() + " but got "
											+ line.length + "-sized tuple in .csv file",
									SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}
				PseudoTuple ps = new PseudoTuple(fp.realArity());
				for (int i = 0; i != line.length; ++i) {
					ps.set(i, parseCSV(program, line[i]));
				}
				fp.relation.addTuple(ps);
			}

			SimpleLogger.logger().log("End Read file " + path + " into " + fp, SimpleLogger.LogLevel.Level.DEBUG);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	   Read a relation from a CSV file. In case program is not null, supplementary
	   checks are performed, i.e. when predicate references are imported, it is
	   checked that the refered predicate exists in the program.
     */
	public static Relation readRelationFrom(Program program, File f, int arity) {
		if (!f.exists())
			return null;
		Relation r = new Relation(arity);
		CSVParser parser = new CSVParserBuilder().withSeparator(Compiler.getCSVSeparator()).build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				PseudoTuple ps = new PseudoTuple(arity);
				for (int i = 0; i != line.length; ++i) {
					ps.set(i, parseCSV(program, line[i]));
				}
				r.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return r;
	}

	public static void dumpFileInto(Relation r, File f, boolean append) {
		try (CSVWriter writer = new CSVWriter(new OutputStreamWriter(new FileOutputStream(f, append)),
											  Compiler.getCSVSeparator(),
											  CSVWriter.NO_QUOTE_CHARACTER,
											  CSVWriter.DEFAULT_ESCAPE_CHARACTER,
											  CSVWriter.RFC4180_LINE_END)) {

            SimpleLogger.logger().log("Write Relation: " + f.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
			r.tuples().forEach(t -> {
					writer.writeNext(t.toStringArray());
				});
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void dumpFileInto(FormalPredicate sp, File f) {
		dumpFileInto(sp.relation, f, false);
	}

	public static void dumpFormalPredicatesInto(Program p, File f) {
		p.getFormalPredicates().forEach(sp -> dumpFileInto(sp, f));
	}
}
