package prof;

import java.io.File;
import java.io.IOException;
import java.io.FileWriter;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;

import lang.io.SimpleLogger;

import org.apache.commons.lang3.time.StopWatch;

import org.json.JSONObject;

public class Profile {
    private static Profile PROFILE = new Profile();
    private File output;

    private Map<String, Map<String, Long>> data = new TreeMap<>();
    private Map<String, Map<String, StopWatch>> timers = new LinkedHashMap<>();;

    public static Profile profile() {
		return PROFILE;
    }

    public synchronized void setOutput(File f) {
		SimpleLogger.logger().info("Generating profiling information into file " + f);
		this.output = f;
    }

    public synchronized void startTimer(String group, String timer) {
		if (output == null)
			return;

		Map<String, StopWatch> tg = timers.get(group);
		if (tg == null) {
			tg = new LinkedHashMap<>();
			timers.put(group, tg);
		}

		tg.put(timer, StopWatch.createStarted());
    }

    public synchronized void stopTimer(String group, String timer) {
		if (output == null)
			return;

		StopWatch t = timers.get(group).get(timer);
		t.stop();
		timers.get(group).remove(timer);
		addCounter(group, timer, t.getTime());
    }

    public synchronized void setCounter(String group, String counter, long value) {
		if (output == null)
			return;

		Map<String, Long> cg = data.get(group);
		if (cg == null) {
			cg = new TreeMap<>();
			data.put(group, cg);
		}

		if (cg.put(counter, value) != null) {
			throw new RuntimeException("Profile counter is overwritten.");
		}
    }

	public void incCounter(String group, String counter) {
		addCounter(group, counter, 1);
	}

	public synchronized void addCounter(String group, String counter, long v) {
		if (output == null)
			return;

		Map<String, Long> cg = data.get(group);
		if (cg == null) {
			cg = new TreeMap<>();
			data.put(group, cg);
		}

		Long prev = cg.get(counter);
		if (prev != null) {
			cg.put(counter, prev + v);
		} else {
			cg.put(counter, (long) v);
		}
	}


	public synchronized void writeOut() {
		if (output == null)
			return;

		try (FileWriter fileWriter = new FileWriter(output)) {
			JSONObject json = new JSONObject(data);
			json.write(fileWriter, 2, 0);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
