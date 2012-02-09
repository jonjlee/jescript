package jescript.lexer;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.LinkedList;
import jescript.node.Token;
import jescript.parser.ParserException;
import jescript.preprocessor.node.Start;
import jescript.preprocessor.parser.Parser;

public class PreprocessorLexer extends Lexer {

	private LinkedList<Token> tokens;
	private Start ast;
	
	public PreprocessorLexer(PushbackReader in) throws ParserException, LexerException, IOException {
		super(null);
		jescript.preprocessor.lexer.Lexer l = new jescript.preprocessor.lexer.Lexer(in);
		Parser p = new Parser(l);
		Preprocessor preprocessor = new Preprocessor();
		try {
			ast = p.parse();
		} catch (jescript.preprocessor.parser.ParserException e) {
			try {
				throw new ParserException(Preprocessor.translateToken(e.getToken()), e.getMessage());
			} catch (Exception e1) {
				throw new ParserException(null, e.getMessage());
			}
		} catch (jescript.preprocessor.lexer.LexerException e) {
			throw new LexerException(e.getMessage());
		}
		ast.apply(preprocessor);
		tokens = preprocessor.getTokens();
	}
	public Start getOriginalAst() {
		return ast;
	}
	@Override public Token peek() throws LexerException, IOException {
		return tokens.peek();
	}
	@Override public Token next() throws LexerException, IOException {
		return tokens.remove();
	}
}
