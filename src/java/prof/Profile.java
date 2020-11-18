package prof;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import lang.io.SimpleLogger;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;

import org.apache.commons.lang3.time.StopWatch;

public class Profile {
    private static Profile PROFILE = new Profile();
    private File output;

    private Map<String, Map<String, Long>> data = new TreeMap<>();
    private Map<String, Map<String, StopWatch>> timers = new HashMap<>();;

    public static Profile profile() {
		return PROFILE;
    }

    public void setOutput(File f) {
		SimpleLogger.logger().info("Generating profiling information into file " + f);
		this.output = f;
    }

    public void startTimer(String group, String timer) {
		if (output == null)
			return;

		Map<String, StopWatch> tg = timers.get(group);
		if (tg == null) {
			tg = new HashMap<>();
			timers.put(group, tg);
		}

		tg.put(timer, StopWatch.createStarted());
    }

    public void stopTimer(String group, String timer) {
		if (output == null)
			return;

		StopWatch t = timers.get(group).get(timer);
		t.stop();
		timers.get(group).remove(timer);
		setCounter(group, timer, t.getTime());
    }

    public void setCounter(String group, String counter, long value) {
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

	public void writeOut() {
		if (output == null)
			return;

		try {
			ObjectMapper mapper = new ObjectMapper();
			ObjectWriter writer = mapper.writer(new DefaultPrettyPrinter());
			writer.writeValue(output, data);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
