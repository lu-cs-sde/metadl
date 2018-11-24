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

import lang.ast.Constant;
import lang.ast.FormalPredicate;
import lang.ast.IntConstant;
import lang.ast.Program;
import lang.ast.StringConstant;
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

	public static void readFileInto(Program program, FormalPredicate sp, File f) {
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				if (line.length != sp.realArity()) {
					SimpleLogger
							.logger().log(
									"Predicate: " + sp.predicateName() + " has arity: " + sp.realArity() + " but got "
											+ line.length + "-sized tuple in .csv file",
									SimpleLogger.LogLevel.Level.ERROR);
					System.exit(0);
				}
				PseudoTuple ps = new PseudoTuple(sp.realArity());
				for (int i = 0; i != line.length; ++i) {
					Constant o = isInteger(line[i]) ? new IntConstant(line[i]) : new StringConstant(line[i]);
					program.objects.add(o);
					ps.instantiate(i, o);
				}
				sp.relation.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static Relation readRelationFrom(File f, FormalPredicate fp) {
		if (!f.exists())
			return null;
		Relation r = new Relation(fp.realArity());
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				PseudoTuple ps = new PseudoTuple(fp.realArity());
				for (int i = 0; i != line.length; ++i) {
					ps.instantiate(i, isInteger(line[i]) ? new IntConstant(line[i]) : new StringConstant(line[i]));
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
