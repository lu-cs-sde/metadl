package lang.io;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.nio.file.Path;
import java.nio.file.Paths;

import lang.ast.LangParser;
import lang.ast.LangScanner;
import lang.ast.Program;
import lang.ast.config.ConfigParser;
import lang.ast.config.ConfigScanner;
import lang.ast.config.Description;

public class FileUtil {
	public static String changeExtension(String filename, String newExtension) {
		int index = filename.lastIndexOf('.');
		if (index != -1) {
			return filename.substring(0, index) + newExtension;
		} else {
			return filename + newExtension;
		}
	}
	
	public static String fileName(String path) {
		Path p = Paths.get(path);
		return p.getFileName().toString();
	}
	
	public static String normalize(String dir) {
		if(dir.length() == 0) return dir;
		if(dir.charAt(dir.length() - 1) == '/') return dir.substring(0, dir.length() - 1);
		return dir;
	}
	
	public static Program parse(File file) throws IOException, beaver.Parser.Exception {
		LangScanner scanner = new LangScanner(new FileReader(file));
		LangParser parser = new LangParser();
		return (Program)parser.parse(scanner);
	}
	
	public static Description parseDescription(String s) throws IOException, beaver.Parser.Exception {
		ConfigScanner configScanner = new ConfigScanner(new StringReader(s));
		ConfigParser configParser   = new ConfigParser();
		Description descr = (Description) configParser.parse(configScanner);
		return descr;
	}
}
