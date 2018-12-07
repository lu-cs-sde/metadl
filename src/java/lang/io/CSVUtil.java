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

public class CSVUtil {
	public static boolean isInteger(String s) {
		try {
			Integer.parseInt(s);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}
	
	public static Term parseCSV(String line) {
		if(isInteger(line)) {
			return new IntConstant(line);
		}
		if(line.charAt(0) == '\'') {
			return new PredicateRef(line.substring(1));
		}
		return new StringConstant(line);
	}

	public static void readFileInto(Program program, FormalPredicate fp, String path) {
		SimpleLogger.logger().log("Read file " + path + " into " + fp, SimpleLogger.LogLevel.Level.DEBUG);
		
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
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
					ps.set(i, parseCSV(line[i]));
				}
				fp.relation.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static Relation readRelationFrom(File f, int arity) {
		if (!f.exists())
			return null;
		Relation r = new Relation(arity);
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				PseudoTuple ps = new PseudoTuple(arity);
				for (int i = 0; i != line.length; ++i) {
					ps.set(i, parseCSV(line[i]));
				}
				r.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return r;
	}

	public static void dumpFileInto(FormalPredicate sp, File f) {
		try (CSVWriter writer = new CSVWriter(new OutputStreamWriter(new FileOutputStream(f)),
				CSVWriter.DEFAULT_SEPARATOR, CSVWriter.NO_QUOTE_CHARACTER, CSVWriter.DEFAULT_ESCAPE_CHARACTER,
				CSVWriter.RFC4180_LINE_END)) {

            SimpleLogger.logger().log("Write Relation: " + f.getPath(), SimpleLogger.LogLevel.Level.DEBUG);
			sp.relation.tuples().forEach(t -> {
				writer.writeNext(t.toStringArray());
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void dumpFormalPredicatesInto(Program p, File f) {
		p.getFormalPredicates().forEach(sp -> dumpFileInto(sp, f));
	}
}
