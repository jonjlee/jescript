package jescript.lexer;

import static org.testng.Assert.*;
import jescript.node.EOF;
import jescript.node.TWhitespace;

public class Util {
	public static void testTokens(Lexer lex, String input, Class<?>... expectedTokens) {
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
