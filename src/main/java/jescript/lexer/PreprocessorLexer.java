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
		
		// Parse input using the preprocessor grammar
		jescript.preprocessor.lexer.Lexer l = new jescript.preprocessor.lexer.Lexer(in);
		Parser p = new Parser(l);
		try {
			ast = p.parse();
		} catch (jescript.preprocessor.parser.ParserException e) {
			try {
				throw new ParserException(Preprocessor.translateToken(e.getToken()), e.getMessage());
			} catch (Exception e1) {
				throw new ParserException(null, "Unknown token " + e.getToken().getClass() + " ("+ e.getToken().getText() + "):\n" + e.getMessage());
			}
		} catch (jescript.preprocessor.lexer.LexerException e) {
			throw new LexerException(e.getMessage());
		}
		
		// Preprocessor walks the AST, saving preprocessed tokens
		Preprocessor preprocessor = new Preprocessor();
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
