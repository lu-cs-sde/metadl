package lang.io;

public class SimpleLogger {

	private static LogLevel.Level lastMode = LogLevel.Level.DEBUG;
	private static SimpleLogger logger = new SimpleLogger();

	public static SimpleLogger logger() {
		return logger;
	}

	private static boolean isDebugMode() {
		String mode = System.getenv("METADL_LOG");
		if(mode == null) return false;
		if(!mode.contains("debug")) return false;
		return true;
	}

	private static boolean isTimerMode() {
		String mode = System.getenv("METADL_LOG");
		if(mode == null) return false;
		if(!mode.contains("time")) return false;
		return true;

	}

	private SimpleLogger log(String msg, LogLevel.Level level, String classname, String methodname) {
		lastMode = level;
		if (level == LogLevel.Level.DEBUG && !isDebugMode()) return logger;
		if (level == LogLevel.Level.TIME && !isTimerMode()) return logger;
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

	public SimpleLogger debug(String msg) {
		return log(msg, LogLevel.Level.DEBUG);
	}

	public SimpleLogger time(String msg) {
		return log(msg, LogLevel.Level.TIME);
	}

	public static class LogLevel {

		public enum Level {
			DEBUG, INFO, ERROR, TIME
		}

		public static String toString(Level l) {
			switch (l) {
			case DEBUG:
				return "DEBUG";
			case INFO:
				return "INFO";
			case ERROR:
				return "ERROR";
			case TIME:
				return "TIME";
			default:
				return "BADLEVEL";
			}
		}
	}
}
