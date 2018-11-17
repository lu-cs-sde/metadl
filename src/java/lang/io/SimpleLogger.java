package lang.io;

public class SimpleLogger {

	private static LogLevel.Level lastMode = LogLevel.Level.DEBUG;
	private static SimpleLogger logger = new SimpleLogger();

	public static SimpleLogger logger() {
		return logger;
	}
	
	private static boolean isDebugMode() {
		String mode = System.getenv("DebugMode");
		if(mode == null) return false;
		if(!mode.equals("true")) return false; 
		return true;
	}

	public SimpleLogger log(String msg, LogLevel.Level level) {
		lastMode = level;
		if(level == LogLevel.Level.DEBUG && !isDebugMode()) return logger;
		System.out.println("[" + LogLevel.toString(level) + "]: " + msg);
		return logger;
	}
	
	public SimpleLogger log(String msg) {
		if(lastMode == LogLevel.Level.DEBUG && !isDebugMode()) return logger;
		System.out.println("[" + LogLevel.toString(lastMode) + "]: " + msg);
		return logger;
	}

	public static class LogLevel {

		public enum Level {
			DEBUG, INFO, ERROR
		}

		public static String toString(Level l) {
			switch (l) {
			case DEBUG:
				return "DEBUG";
			case INFO:
				return "INFO";
			case ERROR:
				return "ERROR";
			default:
				return "BADLEVEL";
			}
		}
	}
}
