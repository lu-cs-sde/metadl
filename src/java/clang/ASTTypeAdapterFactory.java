package clang;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
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
import com.google.gson.stream.JsonWriter;

public class ASTTypeAdapterFactory implements TypeAdapterFactory {
	private Map<String, TypeAdapter> delegates = new HashMap<>();
	private Set<Class<?>> astNodeTypes = new HashSet<>();
	private Function<String, String> oracle;

	public <T extends AST.Node> void registerNodeType(Class<T> nodeT) {
		astNodeTypes.add(nodeT);
	}

	public void registerNodeKindFalback(Function<String, String> oracle) {
		this.oracle = oracle;
	}


	@Override public <T> TypeAdapter<T> create(final Gson gson, final TypeToken<T> type) {
		if (type.getRawType() != AST.Node.class) {
			return null;
		}

		TypeAdapter defaultDelegate = gson.getDelegateAdapter(this, TypeToken.get(AST.Node.class));

		for (Class<?> nodeT : astNodeTypes) {
			String name = nodeT.getSimpleName();

			TypeAdapter ta = gson.getDelegateAdapter(ASTTypeAdapterFactory.this, TypeToken.get(nodeT));
			delegates.put(name, ta);
		}

		return new TypeAdapter<T>() {
			@Override public T read(JsonReader reader) throws JsonParseException {
				JsonElement e = Streams.parse(reader);
				JsonObject o = e.getAsJsonObject();

				if (!o.has("kind")) {
					return null; //defaultDelegate.fromJsonTree(e);
				}

				// lookup the map of registered types
				TypeAdapter ta = delegates.get(o.get("kind").getAsString());
				if (ta == null)  {
					String fallbackKind = oracle.apply(o.get("kind").getAsString());
					if (fallbackKind != null) {
						ta = delegates.get(fallbackKind);
					}
					if (ta == null) {
						ta = defaultDelegate;
					}
				}
				return (T) ta.fromJsonTree(e);
			}

			@Override public void write(JsonWriter writer, T t) throws IOException {
				defaultDelegate.write(writer, t);
			}
		};
	}
}
