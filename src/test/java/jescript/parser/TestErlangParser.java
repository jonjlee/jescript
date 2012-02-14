package jescript.parser;

import static jescript.parser.Util.*;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.Arrays;
import jescript.lexer.Lexer;
import jescript.node.AFunClause;
import jescript.node.AFunExpr;
import jescript.node.AModuleAttrExpr;
import jescript.node.AModuleExpr;
import jescript.node.Node;
import jescript.node.PExpr;
import jescript.node.PFunClause;
import jescript.node.Start;
import org.testng.annotations.Test;
import org.testng.log4testng.Logger;

@Test
public class TestErlangParser {

	private static Logger LOG = Logger.getLogger(TestErlangParser.class);

	public void strings() {
		testExpr("\"a\" \"b\" \"c\"", string("abc"));
		testExpr("file, module, export, import, compile, record", 
				atom("file"), atom("module"), atom("export"), atom("import"), atom("compile"), atom("record"));
	}

	private void testExpr(String input, PExpr... ast) {
		Node s = parse("-module(m). f() -> " + input + ".");
		assertValid(s);
		assertASTEquals(s, new AModuleExpr(Arrays.asList(
				(PExpr) new AModuleAttrExpr(atomTok("m")),
				new AFunExpr(Arrays.asList((PFunClause) 
						new AFunClause(atomTok("f"), none, none, Arrays.asList(ast)))))));
	}
	private Node parse(String input) {
		try {
			Lexer l = new Lexer(new PushbackReader(new StringReader(input), 1024));
			Parser p = new ErlangParser(l);
			Start s = p.parse();
			LOG.debug("\n" + input + " = \n" + nodeToString(s));
			return s;
		} catch (Exception e) {
			return new ParseFailed(input, e);
		}
	}
}
