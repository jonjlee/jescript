package jescript.lexer;

import java.io.PushbackReader;
import java.io.StringReader;
import jescript.node.TAtom;
import jescript.node.TDash;
import jescript.node.TDot;
import jescript.node.TLparen;
import jescript.node.TModule;
import jescript.node.TRparen;
import org.testng.annotations.Test;

@Test
public class TestPreprocessorLexer {

	public void emptyModule() {
		testTokens("-module(m).", TDash.class, TModule.class, TLparen.class, TAtom.class, TRparen.class, TDot.class);
	}
	
	public void constMacro() {
		testTokens("-define(C, x). ?C.", TAtom.class, TDot.class);
	}
	
	public void funMacro() {
		testTokens("-define(C(), x). ?C().", TAtom.class, TDot.class);
	}

	private Lexer initLexer(String input) {
		try {
			return new PreprocessorLexer(new PushbackReader(new StringReader(input), 1024));
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void testTokens(String input, Class<?>... expectedTokens) {
		Util.testTokens(initLexer(input), input, expectedTokens);
	}
}
