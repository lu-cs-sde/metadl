package printing;

import lang.ast.ASTNode;
import lang.ast.List;

public class CollectUtil<T extends ASTNode> {

	public void collectList(String pre, String post, String delim, StringBuilder sb, List<T> list,
			Collector<T> collector) {
		if (list.getNumChild() == 0)
			return;
		sb.append(pre);
		collector.collect(list.getChild(0), sb);
		if (list.getNumChild() == 1) {
			sb.append(post);
			return;
		}

		for (int i = 1; i != list.getNumChild(); ++i) {
			sb.append(delim);
			collector.collect(list.getChild(i), sb);
		}
		sb.append(post);
	}

	public interface Collector<T extends ASTNode> {
		void collect(T t, StringBuilder sb);
	}
}
