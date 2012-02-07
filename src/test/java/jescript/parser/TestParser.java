package jescript.parser;

import static org.testng.Assert.*;
import java.io.PushbackReader;
import java.io.StringReader;
import jescript.analysis.DepthFirstAdapter;
import jescript.lexer.Lexer;
import jescript.node.Node;
import jescript.node.Start;
import org.testng.annotations.Test;
import org.testng.log4testng.Logger;

@Test
public class TestParser {

	private static Logger LOG = Logger.getLogger(TestParser.class);

	public void emptyModule() {
		testInput("-module(m).");
		testInvalidInput("-module(X).");
	}
	
	public void headerAttributes() {
		testModule("-export([main/1]).");
		testModule("-import(io, [fwrite/1]).");
		testModule("-compile([]).\n-compile(export_all).\n-compile([export_all, 'E']).");
		testModule("-custom(1.1).");
		testInvalidModule("-custom(1.1, x).");
	}
	
	public void records() {
		testModule("-record(empty, {}).");
		testModule("-record(person, {name, phone, address}).");
		testModule("-record(person, {name = \"\", phone = [], address}).");
	}
	
	public void simpleFuns() {
		testModule("main(X) -> X.");
		testModule("main(x) -> x.");
		testModule("main(1) -> x.");
		testModule("main($\\^c) -> x.");
		testModule("main(\"ab\" \"c\") -> x.");
		testModule("main(_) -> x.");
		testModule("main([]) -> x.");
		testModule("main([1,2]) -> x.");
		testModule("main({a,b,c}) -> x.");
		testModule("main(#person{name=\"Robert\"}) -> x.");

		testModule("main(X, 1, atom) -> X.");
		testInvalidModule("main(,) -> x.");
	}
	
	public void guards() {
		testModule("f(X) when true -> X.");
		testModule("f(X) when true, x -> X.");
		testModule("f(X) when is_atom(X) -> X.");
		testModule("f(X) when is_number(X), X =/= x -> X.");
		testModule("f(X) when (X+1 == 2) -> X.");
	}
	
	private void testInput(String input) { assertValid(parse(input)); }
	private void testModule(String input) { testInput("-module(m).\n" + input); }
	private void testInvalidInput(String input) { assertInvalid(parse(input)); } 
	private void testInvalidModule(String input) { testInvalidInput("-module(m).\n" + input); }

	private void assertValid(Node parseResult) {
		if (parseResult instanceof ParseFailed) {
			ParseFailed r = (ParseFailed) parseResult;
			fail(r.getMessage(), r.e);
		}
	}
	
	private void assertInvalid(Node parseResult) {
		assertInvalid(parseResult, -1, -1, -1, -1);
	}

	private void assertInvalid(Node parseResult, int linestart, int lineend, int colstart, int colend) {
		if (!(parseResult instanceof ParseFailed)) {
			fail("Expected parse error but succeeded");
		}
		ParseFailed r = (ParseFailed) parseResult;
		if (!(r.e instanceof ParserException)) {
			fail("Expected a parse exception, but got " + r.e.getMessage(), r.e);
		}
		String[] pos = r.e.getMessage().split("\\[|\\]|,");
		int line = Integer.parseInt(pos[1]), col = Integer.parseInt(pos[2]);
		assertTrue(linestart < 0 || (linestart <= line && lineend >= line), "\n    Error at unexpected position: " + r.getMessage());
		assertTrue(colstart < 0 || (colstart <= col && colend >= col),      "\n    Error at unexpected position: " + r.getMessage());
	}

	private Node parse(String input) {
		try {
			Lexer l = new Lexer(new PushbackReader(new StringReader(input), 1024));
			Parser p = new Parser(l);
			Start s = p.parse();
			return s;
		} catch (Exception e) {
			return new ParseFailed(input, e);
		}
	}
	
	private Node parseExpr(String expr) {
		//            1        10   15
		Node s = parse("-module(m). " + expr + ".");
		if (!(s instanceof ParseFailed)) {
			LOG.debug(expr + " ->\n" + nodeToString(s));
		}
		return s;
	}

	private String nodeToString(Node s) {
		final StringBuilder ast = new StringBuilder();
		s.apply(new DepthFirstAdapter() {
			int indent = 0;
			void append(Node n) {
				StringBuilder s = new StringBuilder();
				for (int i = 0; i < indent; i++) {
					s.append("|  ");
				}
				s.append(n.getClass().toString().replaceFirst("class appel.ch03.node.A?", "")).append(getText(n));
				ast.append(s).append("\n");
			}
			String getText(Node n) {
				String s = null;
//				if (n instanceof ANumberExpr || n instanceof ABoolExpr || n instanceof ACharExpr || n instanceof AStringExpr || n instanceof AVarExpr) {
//					s = n.toString();
//				} else if (n instanceof AClassDef) {
//					s = ((AClassDef) n).getId().toString();
//				} else if (n instanceof AFunDef) {
//					s = ((AFunDef) n).getId().toString().trim() + "(" + ((AFunDef) n).getFormal().toString() + ")";
//				}
//				if (s != null) {
//					return "(" + s.trim() + ")";
//				}
				return "";
			}
			@Override public void defaultIn(Node node) {
				append(node);
				indent++;
			}
			@Override public void defaultOut(Node node) {
				indent--;
			}
		});
		return ast.toString();
	}
}