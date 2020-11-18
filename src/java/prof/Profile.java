package prof;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.TreeMap;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;

import org.apache.commons.lang3.time.StopWatch;

public class Profile {
    private static Profile INSTANCE = new Profile();
    private File output;

    private Map<String, Map<String, Long>> data = new TreeMap<>();
    private Map<String, Map<String, StopWatch>> timers = new TreeMap<>();;

    public static Profile profile() {
		return INSTANCE;
    }

    public void setOutput(File f) {
		this.output = f;
    }

    public void startTimer(String group, String timer) {
		Map<String, StopWatch> tg = timers.get(group);
		if (tg == null) {
			tg = new TreeMap<>();
			timers.put(group, tg);
		}

		tg.put(timer, StopWatch.createStarted());
    }

    public void stopTimer(String group, String timer) {
		StopWatch t = timers.get(group).get(timer);
		t.stop();
		setCounter(group, timer, t.getTime());
    }

    public void setCounter(String group, String counter, long value) {
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
		try {
			ObjectMapper mapper = new ObjectMapper();
			ObjectWriter writer = mapper.writer(new DefaultPrettyPrinter());
			writer.writeValue(output, data);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
}
