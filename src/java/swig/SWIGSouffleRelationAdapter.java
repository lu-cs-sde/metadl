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

	public void insertTupleDirect(Object... elems) {
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

	@Override public void insertTuple(Object... elems) {
		// arity
		int requiredSize = Integer.BYTES;
		int minRequiredSize = Integer.BYTES;
		for (Object elem : elems) {
			if (elem instanceof Integer ||
				elem instanceof Long) {
				// 8 bytes for value
				requiredSize += Long.BYTES;
				// 4 bytes in the header
				requiredSize += Integer.BYTES;
			} else if (elem instanceof String) {
				// size of the 0 terminated string
				requiredSize += ((String) elem).getBytes(StandardCharsets.UTF_8).length + 1;
				// 4 bytes in the header, for the length
				requiredSize += Integer.BYTES;
			} else {
				throw new RuntimeException("Unknown type for tuple element.");
			}
		}

		if (buffer.remaining() >= requiredSize + minRequiredSize) {
			buffer.putInt(elems.length);
			for (Object elem : elems) {
				if (elem instanceof Integer) {
					buffer.putInt(-1);
					buffer.putLong((long) (int) (Integer)elem);
				} else if (elem instanceof Long) {
					buffer.putInt(-1);
					buffer.putLong((Long) elem);
				} else {
					byte[] bytes = ((String) elem).getBytes(StandardCharsets.UTF_8);
					buffer.putInt(bytes.length + 1);
					buffer.put(bytes);
					buffer.put((byte) 0);
				}
			}
			totalSize += requiredSize;
			totalTuples++;
		} else {
			// mark the end of the buffer
			assert buffer.remaining() >= minRequiredSize;
			buffer.putInt(-1);

			// trigger a read at the other end
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

			// dumpBuffer();
			rel.readTuplesFromBuffer(buffer);

			buffer.clear();

			totalSize += Integer.BYTES;
		}
		System.err.println("==== Added " + totalTuples + " tuples, with a size of " + totalSize + " bytes.");
	}
}
