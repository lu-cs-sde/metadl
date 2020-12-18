package lang.java.obj;

import java.io.IOException;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;

import eval.EvaluationContext;
import eval.Relation2;
import lang.io.CSVUtil;
import lang.io.SQLUtil;
import lang.relation.RelationWrapper;

public class FileIdDatabase implements org.extendj.ast.FileIdStorage {
	private Map<String, Integer> fileToIdMap = new TreeMap<>();
	private Map<Integer, String> idToFileMap = new HashMap<>();

	int runningId = 1;
	@Override synchronized public int getIdForLib(String lib) {
		int ret;
		if (!fileToIdMap.containsKey(lib)) {
			fileToIdMap.put(lib, runningId);
			idToFileMap.put(runningId, lib);
			ret = runningId++;
			//			System.err.println("Assigning " + ret + " id to " + lib);
		}

		ret = fileToIdMap.get(lib);
		return ret;
	}

	@Override public int getIdForFile(String file) {
		return getIdForLib(file);
	}

	public static FileIdDatabase loadFromFile(String path) throws IOException {
		FileIdDatabase ret = new FileIdDatabase();
		CSVUtil.readMap(ret.fileToIdMap, Function.identity(), Integer::valueOf, path);
		for (Map.Entry<String, Integer> e : ret.fileToIdMap.entrySet()) {
			ret.runningId = Integer.max(ret.runningId, e.getValue() + 1);
			ret.idToFileMap.put(e.getValue(), e.getKey());
		}
		return ret;
	}

	public static FileIdDatabase loadFromTable(String path, String table) throws SQLException {
		FileIdDatabase ret = new FileIdDatabase();
		SQLUtil.readMap(ret.fileToIdMap, path, table);
		for (Map.Entry<String, Integer> e : ret.fileToIdMap.entrySet()) {
			ret.runningId = Integer.max(ret.runningId, e.getValue() + 1);
			ret.idToFileMap.put(e.getValue(), e.getKey());
		}
		return ret;
	}

	public void storeToFile(String path) throws IOException {
		CSVUtil.writeMap(fileToIdMap, path);
	}

	public void storeToTable(String path, String table) throws SQLException {
		SQLUtil.writeMap(fileToIdMap, path, table);
	}
}
