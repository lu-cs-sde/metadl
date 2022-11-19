package clang;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.TypeAdapter;
import com.google.gson.TypeAdapterFactory;
import com.google.gson.internal.Streams;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;

public class ASTTypeAdapterFactory implements TypeAdapterFactory {
	private Map<String, Class<?>> astNodeTypes = new HashMap<>();
	private Function<String, String> oracle;

	public <T extends AST.Node> void registerNodeType(Class<T> nodeT) {
		astNodeTypes.put(nodeT.getSimpleName(), nodeT);
	}

	public void registerNodeKindFalback(Function<String, String> oracle) {
		this.oracle = oracle;
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

	AST.Node parseNode(Gson gson, JsonReader reader) throws IOException {
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
			if (reader.hasNext() && reader.nextName().equals("id")) {
				id = reader.nextString();
			}
			if (reader.hasNext() && reader.nextName().equals("kind")) {
				kind = reader.nextString();
			}

			if (id == null || kind == null) {
				throw new JsonParseException("Failed to parse type and id of node.");
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
}
