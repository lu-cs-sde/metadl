package lang;
import beaver.Symbol;
import beaver.Scanner;
import java.util.ArrayList;

public class CTestUtil {
	public static java.util.List<Symbol> scan(Scanner scanner) {
		ArrayList<Symbol> result = new ArrayList<>();
		do {
			beaver.Symbol sym;
			try {
				sym = scanner.nextToken();
			} catch (Exception e) {
				System.out.println(e);
				break;
			}

			if (sym == null || sym.getId() == 0 /*EOF*/) {
				break;
			} else if (sym.getId() < 0) {
				// layout symbols, ignore
			} else {
				result.add(sym);
			}
		} while (true);

		return result;
	}


}
