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

import org.apache.commons.lang3.time.StopWatch;

import eval.EvaluationContext;
import eval.Relation2;
import eval.Tuple;
import lang.ast.FormalPredicate;
import lang.ast.IntConstant;
import lang.ast.IntegerType;
import lang.ast.PredicateRef;
import lang.ast.PredicateRefType;
import lang.ast.PredicateType;
import lang.ast.Program;
import lang.ast.StringConstant;
import lang.ast.StringType;
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

	public static void readRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path) throws IOException {
		assert t.arity() == rel.arity();

		SimpleLogger.logger().log("Read file " + path, SimpleLogger.LogLevel.Level.DEBUG);

		CSVParser parser = new CSVParserBuilder().withSeparator(Compiler.getCSVSeparator()).build();
		CSVReader reader = new CSVReaderBuilder(new FileReader(new File(path))).withCSVParser(parser).build();

		String[] line;
		while ((line = reader.readNext()) != null) {
			if (line.length != rel.arity()) {
				SimpleLogger
					.logger().error(
									"Relation " + rel.getName() + " has arity " + rel.arity() + " but got "
									+ line.length + "-sized tuple in .csv file");
				throw new RuntimeException("Error while loading relation from file " + path);
			}

			Tuple tup = new Tuple(rel.arity());
			for (int i = 0; i < line.length; ++i) {
				if (t.get(i) == IntegerType.get()) {
					tup.set(i, Long.parseLong(line[i]));
				} else if (t.get(i) == StringType.get()) {
					tup.set(i, ctx.internalizeString(line[i]));
				} else {
					assert t.get(i) == PredicateRefType.get();
					if (line[i].isEmpty() || line[i].charAt(0) != '\'') {
						throw new RuntimeException("Invalid CSV entry for PredicateRefType");
					}
					tup.set(i, ctx.internalizeString(line[i]));
				}
			}

			rel.insert(tup);
		}

		SimpleLogger.logger().log("End Read file " + path + " into " + rel.getName(), SimpleLogger.LogLevel.Level.DEBUG);
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


	public static void writeRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path) throws IOException {
		StopWatch timer = StopWatch.createStarted();

		CSVWriter writer = new CSVWriter(new OutputStreamWriter(new FileOutputStream(path)),
										 Compiler.getCSVSeparator(),
										 CSVWriter.NO_QUOTE_CHARACTER,
										 CSVWriter.DEFAULT_ESCAPE_CHARACTER,
										 CSVWriter.RFC4180_LINE_END);

		SimpleLogger.logger().log("Write Relation: " + path, SimpleLogger.LogLevel.Level.DEBUG);

		String[] line = new String[t.arity()];
		for (Tuple tup : rel.tuples()) {
			for (int i = 0; i < t.arity(); ++i) {
				if (t.get(i) == IntegerType.get()) {
					line[i] = Long.toString(tup.get(i));
				} else if (t.get(i) == StringType.get()) {
					line[i] = ctx.externalizeString(tup.get(i));
				} else {
					assert t.get(i) == PredicateRefType.get();
					line[i] = ctx.externalizeString(tup.get(i));
				}

			}
			writer.writeNext(line);
		}

		timer.stop();
		SimpleLogger.logger().time("Writing CSV file: " + timer.getTime() + "ms ");
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
		StopWatch timer = StopWatch.createStarted();

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

		timer.stop();
		SimpleLogger.logger().time("Writing CSV file: " + timer.getTime() + "ms ");
	}

	public static void dumpFileInto(FormalPredicate sp, File f) {
		dumpFileInto(sp.relation, f, false);
	}

	public static void dumpFormalPredicatesInto(Program p, File f) {
		p.getFormalPredicates().forEach(sp -> dumpFileInto(sp, f));
	}
}
