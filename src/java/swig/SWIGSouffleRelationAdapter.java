package swig;

import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;

import lang.relation.TupleInserter;

public class SWIGSouffleRelationAdapter implements TupleInserter {
	private final static int BUFFER_SIZE = 4 << 20;
	private long totalTuples = 0;
	private long totalSize = 0;

	private SWIGSouffleRelation rel;
	private ByteBuffer buffer = ByteBuffer.allocateDirect(BUFFER_SIZE);
	public SWIGSouffleRelationAdapter(SWIGSouffleRelation rel) {
		this.rel = rel;
		buffer.order(ByteOrder.nativeOrder());
	}
	@Override
	public void insertTuple(Object... elems) {
		assert false;
		SWIGSouffleTuple tpl = rel.makeTuple();
		for (Object elem : elems) {
			if (elem instanceof Long) {
				tpl.add((long) elem);
			} else if (elem instanceof String) {
				tpl.add((String) elem);
			} else if (elem instanceof Integer) {
				tpl.add((long) (int) elem);
			} else {
				throw new RuntimeException("Unknown type for tuple element.");
			}
		}
		rel.add(tpl);
	}

	@Override
	public void insertTuple(String s0, long l1, long l2, long l3, String s4) {
		byte[] s0Bytes = s0.getBytes(StandardCharsets.UTF_8);
		byte[] s4Bytes = s4.getBytes(StandardCharsets.UTF_8);
		// the strings should be 0-terminated, that means 2 extra bytes in the
		// output

		int requiredSize = 2 * Integer.BYTES + // s0len, s4len
			1 + s0Bytes.length + // s0 + terminating 0
			1 + s4Bytes.length + // s4 + terminating 0
			3 * Long.BYTES; // l1, l2, l3

		int minRequiredSize = 2 * Integer.BYTES;

		if (buffer.remaining() >= requiredSize + minRequiredSize) {
			buffer.putInt(s0Bytes.length + 1);
			buffer.putInt(s4Bytes.length + 1);
			buffer.putLong(l1);
			buffer.putLong(l2);
			buffer.putLong(l3);
			buffer.put(s0Bytes);
			buffer.put((byte) 0);
			buffer.put(s4Bytes);
			buffer.put((byte) 0);

			totalTuples++;
			totalSize += requiredSize;

		} else {
			assert buffer.remaining() >= minRequiredSize;
			buffer.putInt(-1);
			buffer.putInt(-1);

			// TODO: we could use double buffering and let
			// this be executed by another thread
			// dumpBuffer();
			rel.readTuplesFromBuffer(buffer);

			// now clear the buffer
			buffer.clear();

			totalSize += minRequiredSize;
		}
	}

	private void dumpBuffer() {
		try (FileOutputStream fos = new FileOutputStream("buf.bin")) {
			fos.getChannel().write(buffer);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public void done() {
		if (buffer.position() > 0) {
			// something has been written in the buffer, trigger a read on
			// the souffle side
			buffer.putInt(-1);
			buffer.putInt(-1);

			// dumpBuffer();
			rel.readTuplesFromBuffer(buffer);

			buffer.clear();

			totalSize += 2 * Integer.BYTES;
		}

		System.err.println("==== Added " + totalTuples + " tuples, with a size of " + totalSize + " bytes.");
	}
}
