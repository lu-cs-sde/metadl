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
import lang.ast.IntConstant;
import lang.ast.Program;
import lang.ast.StringConstant;
import lang.ast.SuperPredicate;
import lang.relation.PseudoTuple;

public class CSVUtil {
	public static boolean isInteger(String s) {
		try {
			Integer.parseInt(s);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	public static void readFileInto(Program program, SuperPredicate sp, File f) {
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
		try (CSVReader reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build()) {
			String[] line;
			while ((line = reader.readNext()) != null) {
				if (line.length != sp.realArity()) {
					System.out.println("Predicate: " + sp.predicateName() + " has arity: " + sp.realArity()
							+ " but got " + line.length + "-sized tuple in .csv file");
					System.exit(0);
				}
				PseudoTuple ps = new PseudoTuple(sp);
				for (int i = 0; i != line.length; ++i) {
					Constant o;
					if (isInteger(line[i])) {
						o = new IntConstant(line[i]);
					} else {
						o = new StringConstant(line[i]);
					}
					program.objects.add(o);
					ps.instantiate(i, o);
				}
				sp.relation.addTuple(ps);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static void dumpFileInto(SuperPredicate sp, File f) {
		try (CSVWriter writer = new CSVWriter(new OutputStreamWriter(new FileOutputStream(f)))) {
			sp.relation.tuples().forEach(t -> {
				writer.writeNext(t.toStringArray());
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
