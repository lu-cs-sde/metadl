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

	private SimpleLogger log(String msg, LogLevel.Level level, String classname, String methodname) {
		lastMode = level;
		if(level == LogLevel.Level.DEBUG && !isDebugMode()) return logger;
		System.out.println("[" + LogLevel.toString(level) + " (" + classname + "@" + methodname + ")]: " + msg);
		return logger;
	}

	public SimpleLogger log(String msg, LogLevel.Level level) {
		StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
		StackTraceElement prev = stackTraceElements[2];
		log(msg, level, prev.getClassName(), prev.getMethodName());
		return logger;
	}

	public SimpleLogger log(String msg) {
		StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
		StackTraceElement prev = stackTraceElements[2];
		log(msg, lastMode, prev.getClassName(), prev.getMethodName());
		return logger;
	}

	public SimpleLogger error(String msg) {
		return log(msg, LogLevel.Level.ERROR);
	}

	public SimpleLogger info(String msg) {
		return log(msg, LogLevel.Level.INFO);
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
