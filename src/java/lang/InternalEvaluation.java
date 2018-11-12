package lang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;

import lang.ast.Constant;
import lang.ast.ExtensionalDB;
import lang.ast.IntConstant;
import lang.ast.List;
import lang.ast.Program;
import lang.ast.StringConstant;
import lang.ast.SuperPredicate;
import lang.relation.PseudoTuple;
import lang.relation.Relation;

public abstract class InternalEvaluation extends Evaluation {

	private boolean isInteger(String s) {
		try {
			Integer.parseInt(s);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	private void readFileInto(Program program, SuperPredicate sp, File f) throws IOException {
		CSVParser parser = new CSVParserBuilder().withSeparator(',').build();
		CSVReader reader = null;

		try {
			reader = new CSVReaderBuilder(new FileReader(f)).withCSVParser(parser).build();
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
		reader.close();
	}

	protected void loadEBDFacts(Program program) {
		program.objects.addAll(program.uniqueFileObjects());
		
		System.out.println("Load EDB Facts");
		List<SuperPredicate> spreds = program.getSuperPredicateList();
		spreds.forEach(sp -> {
			sp.relation = new Relation(sp.fileRelation());

			sp.edbPredicates().forEach(ps -> {
				ExtensionalDB edb = (ExtensionalDB) ps.literal().stmt();
				String fn = edb.getFileLocation().getSTRING();
				File f = new File(fn);

				if (!f.exists()) {
					System.err.println("Missing EDB File: " + fn);
					System.exit(0);
				}

				System.out.println("Read in: " + fn + " into relation: " + sp.predicateName());

				try {
					readFileInto(program, sp, f);
				} catch (FileNotFoundException e) {
					e.printStackTrace();
					System.exit(0);
				} catch (IOException e) {
					e.printStackTrace();
					System.exit(0);
				}
			});
		});
	}
}
