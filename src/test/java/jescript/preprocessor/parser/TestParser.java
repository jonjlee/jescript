package jescript.preprocessor.parser;

import static org.testng.Assert.*;
import java.io.PushbackReader;
import java.io.StringReader;
import jescript.preprocessor.analysis.DepthFirstAdapter;
import jescript.preprocessor.lexer.Lexer;
import jescript.preprocessor.node.Node;
import jescript.preprocessor.node.Start;
import jescript.preprocessor.node.Switch;
import jescript.preprocessor.node.Token;
import org.testng.annotations.Test;
import org.testng.log4testng.Logger;

@Test
public class TestParser {

	private static Logger LOG = Logger.getLogger(TestParser.class);

	public void define() {
		testModule("-define(x,).");
		testModule("-define(_,).");
		testModule("-define(x,x).");
		testModule("-define(CONST,\"abcd\").");

		testModule("-define(x,x))."); // even number of closing parens
		testModule("-define(x,x X \"ab\ncd\")))).");
		testModule("-\t\ndefine ( \tx , x\t\n  )))). )\n .");
	}

	public void ignoresComments() {
		String program = 
				"-module(m).\n" +
				"-define(CONST,\"ab\" % a comment\n" +
				"              \"cd\").";
			String processed = program.replaceAll("%[^\n]*",	"");
			Node s = parse(program);
			assertValid(s);
			assertEquals(nodeToString(s), processed);
	}
	
	public void constantMacros() {
		String program = 
			"-module(m).\n" +
			"-define(CONST,\"ab\" \n" +
			"              \"cd\").\n" +
			"f(X) -> io:format(\"~s ~p\", [?CONST, X]).\n" +
			"-define(CONST2,x).\n" +
			"g(X) -> io:format(\"~s ~p\", [?CONST, ?CONST2]).\n";
		String processed = program;
		//String processed = program.replaceAll("?CONST",	"\"ab\" \n              \"cd\"");
		//processed = program.replaceAll("?CONST2",	"x");
		Node s = parse(program);
		assertValid(s);
		assertEquals(nodeToString(s), processed);
	}

	private void testInput(String input) { assertValid(parse(input)); }
	private void testModule(String input) { testInput("-module(m).\n" + input); }

	private void assertValid(Node parseResult) {
		if (parseResult instanceof ParseFailed) {
			ParseFailed r = (ParseFailed) parseResult;
			fail(r.getMessage(), r.getException());
		}
	}

	private Node parse(String input) {
		try {
			Lexer l = new Lexer(new PushbackReader(new StringReader(input), 1024));
			Parser p = new Parser(l);
			Start s = p.parse();
			LOG.debug("\n" + input + " = \n" + nodeToString(s) + " = \n" + printAst(s));
			return s;
		} catch (Exception e) {
			return new ParseFailed(input, e);
		}
	}

	private String nodeToString(Node n) {
		final StringBuilder s = new StringBuilder();
		n.apply(new DepthFirstAdapter() {
			@Override public void defaultCase(Node node) {
				if (node instanceof Token)
					s.append(((Token) node).getText());
			}
		});
		return s.toString();
	}
	private String printAst(Node s) {
		final StringBuilder ast = new StringBuilder();
		s.apply(new DepthFirstAdapter() {
			int indent = 0;
			void append(Node n) {
				StringBuilder s = new StringBuilder();
				for (int i = 0; i < indent; i++) {
					s.append("|  ");
				}
				s.append(n.getClass().toString().replaceFirst("class jescript.preprocessor.node.A?", "")).append(getText(n));
				ast.append(s).append("\n");
			}
			String getText(Node n) {
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

class ParseFailed extends Token {
	jescript.parser.ParseFailed parseFailed;
	public ParseFailed(String input, Exception e) {
		parseFailed = new jescript.parser.ParseFailed(input, e);
	}
	public String getMessage() {
		return parseFailed.getMessage();
	}
	public Exception getException() {
		return parseFailed.getException();
	}
	@Override public void apply(Switch sw) { throw new UnsupportedOperationException(); }
	@Override public Object clone() { throw new UnsupportedOperationException(); }
}