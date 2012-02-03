package jescript.lexer;

import static org.testng.Assert.*;
import java.io.PushbackReader;
import java.io.StringReader;
import jescript.node.*;
import org.testng.annotations.Test;

@Test
public class TestLexer {
	
	public void separators() {
		testTokens("( ) { } . ] [", TLparen.class, TRparen.class, TLbrace.class, TRbrace.class, TDot.class, TRbrack.class, TLbrack.class);
		testTokens(": | || ; , ? ->", TColon.class, TBar.class, TBarbar.class, TSemi.class, TComma.class, TQuestion.class, TArrow.class);
		testTokens("# << >> _", TPound.class, TLtlt.class, TGtgt.class, TUnderscore.class);
	}
	
	public void nil() {
		testTokens("[] [  ] [ \t\n ]", TNil.class, TNil.class, TNil.class);
	}

	public void reserved() {
		// See Reference Manual, S1.5 (http://www.erlang.org/doc/reference_manual/introduction.html) 
		testTokens("after and andalso band begin bnot bor bsl bsr bxor", TAfter.class, TAnd.class, TAndalso.class, TBand.class, TBegin.class, TBnot.class, TBor.class, TBsl.class, TBsr.class, TBxor.class);
		testTokens("case catch cond div end fun if let not of or orelse", TCase.class, TCatch.class, TCond.class, TDiv.class, TEnd.class, TFun.class, TIf.class, TLet.class, TNot.class, TOf.class, TOr.class, TOrelse.class);
		testTokens("query receive rem try when xor", TQuery.class, TReceive.class, TRem.class, TTry.class, TWhen.class, TXor.class);
	}
	
	public void operators() {
		testTokens("+ - * /", TPlus.class, TDash.class, TStar.class, TSlash.class);
		testTokens("= == /=", TEq.class, TEqeq.class, TNeq.class);
		testTokens("< > <= >=", TLt.class, TGt.class, TLe.class, TGe.class);
		testTokens("++ -- ! <-", TPlusplus.class, TMinusminus.class, TBang.class, TLarrow.class);
	}

	public void numbers() {
		testTokens("0 1 10", TInteger.class, TInteger.class, TInteger.class);
		testTokens("1.0 0.1 1.111", TFloat.class, TFloat.class, TFloat.class);
		testTokens("1.0e1 1.0e+1 1.0e-1 0.1e10 1.0e+10", TFloat.class, TFloat.class, TFloat.class, TFloat.class, TFloat.class);
	}

	public void chars() {
		testTokens("$0 $a $\\a", TChar.class, TChar.class, TChar.class);
		testTokens("$\\0 $\\01 $\\777", TChar.class, TChar.class, TChar.class);
		testTokens("$\\x00 $\\xFF", TChar.class, TChar.class);
		testTokens("$\\^c $\\^Z", TChar.class, TChar.class);
	}

	public void strings() {
		testTokens("\"\" \"x\"", TString.class, TString.class);
		testTokens("\"embedded \\\"quotes\\\"\"", TString.class);
	}
	
	public void atoms() {
		testTokens("a abc", TAtom.class, TAtom.class);
		testTokens("'' 'a' '_a' '0123' 'a b c'", TAtom.class, TAtom.class, TAtom.class, TAtom.class, TAtom.class);
		testTokens("'a \\n \\012 \\xFF \\^c'", TAtom.class);
	}

	public void variables() {
		testTokens("_a _A A Abc", TVar.class, TVar.class, TVar.class, TVar.class);
	}

	public void binaries() {
		
	}

	public void comments() {
		testTokens("%", TComment.class);
		testTokens("%\n", TComment.class);
		testTokens("% ", TComment.class);
		testTokens("% this is a comment % ", TComment.class);
		testTokens( "% mutli \n\n% lines", TComment.class, TComment.class);
		testTokens("X     % inline", TVar.class, TComment.class);
	}

	private Lexer initLexer(String input) {
		Lexer lex = new Lexer(new PushbackReader(new StringReader(input)));
		return lex;
	}

	private void testTokens(String input, Class<?>... expectedTokens) {
		Lexer lex = initLexer(input);
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
