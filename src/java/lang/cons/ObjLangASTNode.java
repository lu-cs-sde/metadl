package lang.cons;

import lang.ast.AnalyzeContext;
import java.util.TreeSet;
import java.io.PrintStream;
import java.util.ArrayList;
/**
   Interface implemented by ASTNode subclasses that can be
   replaced by metavariables.
 */
public interface ObjLangASTNode {
	/**
	   This node's parent
	 */
	ObjLangASTNode getParent();

	/**
	   Name of the variable used to index the current node in a list.
	   May be null for nodes that are not allowed inside List AST nodes
	 */
	String indexVarName();

	/**
	   Name of the variable used to refer to the current node
	 */
	String varName();

	/**
	   Unique ID for this node
	 */
	int getNodeId();

	/**
	   Set the unique IDs for this node and its children
	 */
	void assignIds();

	/**
	   The name of the representative relation for this node type
	 */
	String getRelation();

	/**
	   Common interface for relational pattern representation
	 */
	TreeSet<String> metavariables();
	ArrayList<lang.ast.CommonLiteral> literals(AnalyzeContext ctx, lang.ast.CommonVariable root);

	/**
	   Debug printing
	*/
	void debugPrint(PrintStream ps);
}
