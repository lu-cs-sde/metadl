package lang.io;

import java.io.IOException;

import eval.EvaluationContext;
import eval.Relation2;
import eval.Tuple;
import lang.ast.IntegerType;
import lang.ast.PredicateRefType;
import lang.ast.PredicateType;
import lang.ast.StringType;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class SQLUtil {
	private static String tableType(PredicateType t) {
		if (t.arity() == 0)
			throw new RuntimeException("Unclear how to create tables of 0 arity");

		String ret = "(";
		for (int i = 0; i < t.arity(); i++) {
			if (i != 0)
				ret += ", ";
			ret += "c" + i + " ";
			if (t.get(i).storageType() == IntegerType.get()) {
				ret += "INTEGER";
			} else {
				ret += "TEXT";
			}
		}
		ret += ")";

		return ret;
	}

	private static String tuple(int arity) {
		if (arity == 0)
			throw new RuntimeException("Unclear how to create tables of 0 arity");

		String ret = "(";
		for (int i = 0; i < arity; i++) {
			if (i != 0)
				ret += ", ";
			ret += "?";
		}
		ret += ")";

		return ret;
	}

	public static void writeRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path, String table) throws SQLException {
		Connection conn = DriverManager.getConnection("jdbc:sqlite:" + path);
		Statement tbl = conn.createStatement();
		tbl.executeUpdate("DROP TABLE IF EXISTS " + table);
		tbl.executeUpdate(String.format("CREATE TABLE %s %s", table, tableType(t)));

		PreparedStatement ps = conn.prepareStatement(String.format("INSERT INTO %s VALUES %s", table, tuple(t.arity())));

		for (Tuple tpl : rel.tuples()) {
			for (int i = 0; i < t.arity(); ++i) {
				if (t.get(i).storageType() == IntegerType.get()) {
					ps.setLong(i + 1, tpl.get(i));
				} else if (t.get(i).storageType() == StringType.get()) {
					ps.setString(i + 1, ctx.externalizeString(tpl.get(i)));
				} else {
					assert t.get(i).storageType() == PredicateRefType.get();
					ps.setString(i + 1, "'" + ctx.externalizeString(tpl.get(i)));
				}
			}
			ps.execute();
		}

		conn.close();
	}

	public static void readRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path, String table) throws SQLException {
		Connection conn = DriverManager.getConnection("jdbc:sqlite:" + path);

		Statement select = conn.createStatement();
		ResultSet rs = select.executeQuery("SELECT * FROM " + table);

		Tuple tup = new Tuple(t.arity());
		while (rs.next()) {
			for (int i = 0; i < t.arity(); i++) {
				if (t.get(i).storageType() == IntegerType.get()) {
					tup.set(i, rs.getLong(i + 1));
				} else if (t.get(i).storageType() == StringType.get()) {
					tup.set(i, ctx.internalizeString(rs.getString(i + 1)));
				} else {
					assert t.get(i).storageType() == PredicateRefType.get();
					if (rs.getString(i + 1).isEmpty() || rs.getString(i + 1).charAt(0) != '\'') {
						throw new RuntimeException("Invalid DB entry for PredicateRefType");
					}
					tup.set(i, ctx.internalizeString(rs.getString(i + 1).substring(1)));
				}
			}
			rel.insert(tup);
		}
	}
}
