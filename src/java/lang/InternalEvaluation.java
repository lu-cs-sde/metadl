package lang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import com.opencsv.CSVParser;
import com.opencsv.CSVParserBuilder;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;

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

	private void readFileInto(SuperPredicate sp, File f) throws IOException {
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
				PseudoTuple ps = new PseudoTuple(sp.realArity());
				for (int i = 0; i != line.length; ++i) {
					System.out.println(line[i]);
					if(isInteger(line[i])) {
						ps.instantiate(i, new IntConstant(line[i]));
					} else {
						ps.instantiate(i, new StringConstant(line[i]));
					}
				}
				System.out.println("Add Tuple to: " + sp.predicateName());
				sp.relation.addTuple(ps);
				System.out.println(sp.relation.size());
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		reader.close();
	}

	protected void loadEBDFacts(Program p) {
		System.out.println("Load EDB Facts");
		List<SuperPredicate> spreds = p.getSuperPredicateList();
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
					readFileInto(sp, f);
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
