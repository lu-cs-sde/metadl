package lang.io;

import java.io.IOException;

import eval.EvaluationContext;
import eval.Relation2;
import lang.relation.RelationWrapper;
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
	private static String tableType(PredicateType t, Integer tag) {
		if (t.arity() == 0)
			throw new RuntimeException("Unclear how to create tables of 0 arity");

		String ret = "(";
		for (int i = 0; i < t.arity(); i++) {
			if (i != 0)
				ret += ", ";
			ret += "'" + i + "' ";
			if (t.get(i).storageType() == IntegerType.get()) {
				ret += "INTEGER";
			} else {
				ret += "TEXT";
			}
		}

		if (tag != null) {
			ret += ", _tag INTEGER";
		}

		ret += ")";

		return ret;
	}

	private static String tuple(int arity, Integer tag) {
		if (arity == 0)
			throw new RuntimeException("Unclear how to create tables of 0 arity");

		String ret = "(";
		for (int i = 0; i < arity; i++) {
			if (i != 0)
				ret += ", ";
			ret += "?";
		}

		if (tag != null) {
			ret += ", " + tag;
		}

		ret += ")";

		return ret;
	}

	public static void writeRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path, String table, Integer tag) throws SQLException {
		Connection conn = DriverManager.getConnection("jdbc:sqlite:" + path);
		Statement tbl = conn.createStatement();

		conn.setAutoCommit(false);

		tbl.executeUpdate(String.format("CREATE TABLE IF NOT EXISTS %s %s", table, tableType(t, tag)));
		if (tag != null) {
			tbl.executeUpdate(String.format("CREATE INDEX IF NOT EXISTS '%s_index' ON '%s'(_tag)", table, table));
		}

		PreparedStatement ps = conn.prepareStatement(String.format("INSERT INTO %s VALUES %s", table, tuple(t.arity(), tag)));

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
			ps.addBatch();
		}

		ps.executeBatch();

		conn.commit();
		conn.close();
	}

	public static void readRelation(EvaluationContext ctx, PredicateType t, Relation2 rel, String path, String table, Integer tag) throws SQLException {
		Connection conn = DriverManager.getConnection("jdbc:sqlite:" + path);

		Statement select = conn.createStatement();
		String query = "SELECT * FROM '" + table + "'";
		if (tag != null) {
			query += " WHERE _tag = " + tag;
		}
		ResultSet rs = select.executeQuery(query);

		while (rs.next()) {
			Tuple tup = new Tuple(t.arity());
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

		conn.close();
	}

	public static RelationWrapper readRelation(String path, String table, PredicateType t) throws SQLException {
		Relation2 rel = new Relation2(t.arity());
		EvaluationContext ctx = new EvaluationContext();
		readRelation(ctx, t, rel, path, table, null);
		RelationWrapper result = new RelationWrapper(ctx, rel, t);
		return result;
	}

	public static void writeRelation(String path, String table, RelationWrapper rel) throws SQLException {
		writeRelation(rel.getContext(), rel.type(), rel.getRelation(), path, table, null);
	}
}
