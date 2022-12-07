package clang;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayDeque;
import java.util.Deque;
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

import clang.AST.TranslationUnitDecl;

public class ASTTypeAdapterFactory implements TypeAdapterFactory {
	private Map<String, Class<?>> astNodeTypes = new HashMap<>();
	private Function<String, String> oracle;
	private Predicate<String> fileFilter = (String s) -> true;
	private AST.BareLoc prevLoc = new AST.BareLoc();
	private Deque<AST.Node> parentStack = new ArrayDeque<>();
	private TypeAdapter bareLocAdapter;


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
		if (type.getRawType() == AST.Node.class) {
			TypeAdapter defaultDelegate = gson.getDelegateAdapter(this, TypeToken.get(AST.Node.class));
			return new TypeAdapter<T>() {
				@Override public T read(JsonReader reader) throws IOException {
					return (T) parseNode(gson, reader);
				}

				@Override public void write(JsonWriter writer, T t) throws IOException {
					defaultDelegate.write(writer, t);
				}
			};
		} else if (type.getRawType() == AST.Loc.class) {
			TypeAdapter defaultDelegate = gson.getDelegateAdapter(this, TypeToken.get(AST.Loc.class));
			bareLocAdapter = gson.getAdapter(AST.BareLoc.class);
			return new TypeAdapter<T>() {
				@Override public T read(JsonReader reader) throws IOException {
					return (T) parseLoc(gson, reader);
				}

				@Override public void write(JsonWriter writer, T t) throws IOException {
					defaultDelegate.write(writer, t);
				}
			};
		} else {
			// use the defaults
			return null;
		}
	}

	private AST.Loc parseLoc(Gson gson, JsonReader reader) throws IOException {
		if (reader.peek() == JsonToken.NULL) {
			reader.nextNull();
			return null;
		}

		reader.beginObject();
		if (!reader.hasNext()) {
			reader.endObject();
			return new AST.Loc();
		}

		AST.Loc ret = new AST.Loc();

		while (reader.hasNext()) {
			String fieldName = reader.nextName();
			switch (fieldName) {
			case "spellingLoc":
				ret.spellingLoc = (AST.BareLoc) bareLocAdapter.read(reader);
				prevLoc = ret.spellingLoc.patch(prevLoc);
				break;
			case "expansionLoc":
				ret.expansionLoc = (AST.BareLoc) bareLocAdapter.read(reader);
				prevLoc = ret.expansionLoc.patch(prevLoc);
				break;
			case "line":
				if (ret.loc == null)
					ret.loc = new AST.BareLoc();
				ret.loc.line = reader.nextInt();
				break;
			case "col":
				if (ret.loc == null)
					ret.loc = new AST.BareLoc();
				ret.loc.col = reader.nextInt();
				break;
			case "file":
				if (ret.loc == null)
					ret.loc = new AST.BareLoc();
				ret.loc.file = reader.nextString();
				break;
			default:
				// ignore
				reader.skipValue();
				break;
			}
		}

		if (ret.loc != null) {
			prevLoc = ret.loc.patch(prevLoc);
		}

		reader.endObject();
		return ret;
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

				AST.Node parent = parentStack.peekFirst();

				parentStack.addFirst(node);
				while (reader.hasNext()) {
					String fieldName = reader.nextName();
					try {
						Field field = astNodeType.getField(fieldName);
						TypeAdapter t = gson.getAdapter(field.getType());
						Object o = t.read(reader);
						field.set(node, o);


						if (fieldName.equals("loc")) {
							if (parent instanceof TranslationUnitDecl && node.loc != null
								&& !fileFilter.test(node.loc.getFile())) {
								skipCurrentObject(reader);
								parentStack.removeFirst();
								return null;
							}
						}

						if (fieldName.equals("range")) {
							if (parent instanceof TranslationUnitDecl && node.range != null
								&& node.range.begin != null
								&& !fileFilter.test(node.range.begin.getFile())) {
								skipCurrentObject(reader);
								parentStack.removeFirst();
								return null;
							}

							if (parent instanceof TranslationUnitDecl && node.range != null
								&& node.range.end != null
								&& !fileFilter.test(node.range.end.getFile())) {
								skipCurrentObject(reader);
								parentStack.removeFirst();
								return null;
							}
						}
					} catch (NoSuchFieldException e) {
						reader.skipValue();
					}
				}

				reader.endObject();
				parentStack.removeFirst();
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
