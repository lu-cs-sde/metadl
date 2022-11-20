package clang;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.TypeAdapterFactory;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

import org.apache.commons.collections.functors.IfClosure;

public class ASTTypeAdapterFactory implements TypeAdapterFactory {
	private Map<String, Class<?>> astNodeTypes = new HashMap<>();
	private Function<String, String> oracle;
	private Predicate<String> fileFilter = (String s) -> true;
	AST.Loc prevLoc;

	public <T extends AST.Node> void registerNodeType(Class<T> nodeT) {
		astNodeTypes.put(nodeT.getSimpleName(), nodeT);
	}

	public void registerNodeKindFalback(Function<String, String> oracle) {
		this.oracle = oracle;
	}

	public void registerFileFilter(Predicate<String> filter) {
		this.fileFilter = filter;
	}

	@Override public <T> TypeAdapter<T> create(final Gson gson, final TypeToken<T> type) {
		if (type.getRawType() != AST.Node.class) {
			return null;
		}

		TypeAdapter defaultDelegate = gson.getDelegateAdapter(this, TypeToken.get(AST.Node.class));

		return new TypeAdapter<T>() {
			@Override public T read(JsonReader reader) throws IOException {
				return (T) parseNode(gson, reader);
			}

			@Override public void write(JsonWriter writer, T t) throws IOException {
				defaultDelegate.write(writer, t);
			}
		};
	}

	private AST.Node parseNode(Gson gson, JsonReader reader) throws IOException {
		if (reader.peek() == JsonToken.NULL) {
			reader.nextNull();
			return null;
		}

		reader.beginObject();

		String id = null;
		String kind = null;

		if (!reader.hasNext()) {
			reader.endObject();
			return null;
		} else {
			String nextName = reader.nextName();
			if (reader.hasNext() && nextName.equals("id")) {
				id = reader.nextString();
				nextName = reader.nextName();
			}
			if (nextName.equals("kind")) {
				kind = reader.nextString();
			}

			Class<?> astNodeType = astNodeTypes.get(kind);
			if (astNodeType == null) {
				String fallbackKind = oracle.apply(kind);
				if (fallbackKind != null) {
					astNodeType = astNodeTypes.get(fallbackKind);
					if (astNodeType != null) {
						// map this kind of node to its fallback type
						astNodeTypes.put(kind, astNodeType);
					}
				}
			}

			if (astNodeType == null) {
				astNodeType = AST.Node.class;
			}

			try {
				AST.Node node = (AST.Node) astNodeType.getConstructor().newInstance();
				node.id = id;
				node.kind = kind;
				while (reader.hasNext()) {
					String fieldName = reader.nextName();
					try {
						Field field = astNodeType.getField(fieldName);
						TypeAdapter t = gson.getAdapter(field.getType());
						Object o = t.read(reader);
						field.set(node, o);

						/* Clang AST contains 'differential' source locations and ranges: it contains
						   only the changes relative to the previous node in post-order?.
						   This propagates the location, so that each node has full location description.
						*/

						if (fieldName.equals("loc")) {
							// patch the location
							if (node.loc == null) {
								node.loc = prevLoc;
							} else {
								prevLoc = node.loc.patch(prevLoc);
							}

							if (node.loc != null && node.loc.file != null && !fileFilter.test(node.loc.file)) {
								skipCurrentObject(reader);
								return null;
							}
						}

						if (fieldName.equals("range")) {
							if (node.range == null) {
								node.range = new AST.Range();
							}
							if (node.range.begin == null) {
								node.range.begin = prevLoc;
							} else {
								prevLoc = node.range.begin.patch(prevLoc);
							}

							if (node.range.begin != null && node.range.begin.file != null && !fileFilter.test(node.range.begin.file)) {
								skipCurrentObject(reader);
								return null;
							}


							if (node.range.end == null) {
								node.range.end = prevLoc;
							} else {
								prevLoc = node.range.end.patch(prevLoc);
							}

							if (node.range.end != null && node.range.end.file != null && !fileFilter.test(node.range.end.file)) {
								skipCurrentObject(reader);
								return null;
							}
						}
					} catch (NoSuchFieldException e) {
						reader.skipValue();
					}
				}

				reader.endObject();
				return node;
			} catch (ReflectiveOperationException e ) {
				throw new JsonParseException(e);
			}
		}
	}

	private void skipCurrentObject(JsonReader reader) throws IOException {
		while (reader.hasNext()) {
			reader.skipValue();
		}
		reader.endObject();
	}
}
