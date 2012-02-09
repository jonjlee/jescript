package jescript.lexer;

import static org.testng.Assert.*;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.StringReader;
import jescript.node.EOF;
import jescript.node.TAtom;
import jescript.node.TDash;
import jescript.node.TDot;
import jescript.node.TLparen;
import jescript.node.TModule;
import jescript.node.TRparen;
import jescript.node.TWhitespace;
import jescript.parser.ParserException;
import org.testng.annotations.Test;

@Test
public class TestPreprocessorLexer {

	public void emptyModule() {
		testTokens("-module(m).", TDash.class, TModule.class, TLparen.class, TAtom.class, TRparen.class, TDot.class);
	}
	
	public void constMacro() {
		testTokens("-define(C, x). ?C.", TAtom.class, TDot.class);
	}

	private Lexer initLexer(String input) throws ParserException, LexerException, IOException {
		Lexer lex = new PreprocessorLexer(new PushbackReader(new StringReader(input), 1024));
		return lex;
	}

	private void testTokens(String input, Class<?>... expectedTokens) {
		Lexer lex;
		try {
			lex = initLexer(input);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		Class<?> tokClass;
		int i = 0;
		try {
			for (i = 0; i < expectedTokens.length; i++) {
					// Get next token, skipping whitespace unless explicitly specified in expected token list
					do {
						tokClass = lex.next().getClass();
					} while (tokClass == TWhitespace.class && expectedTokens[i] != TWhitespace.class);
					assertEquals(tokClass, expectedTokens[i], "Token " + i + " of '" + input + "' ");
			}
			// skip over any trailing whitespace
			do {
				tokClass = lex.next().getClass();
			} while (tokClass == TWhitespace.class);
			
			// and assert that the whole string was consumed
			assertEquals(tokClass, EOF.class);
		} catch (Exception e) {
			fail("Unable to get token " + (i+1) + " (of " + expectedTokens.length + ") from " + input, e);
		}
	}
}
