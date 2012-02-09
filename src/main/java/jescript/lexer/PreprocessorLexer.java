package jescript.lexer;

import java.io.IOException;
import java.io.PushbackReader;
import java.util.LinkedList;
import jescript.preprocessor.lexer.Lexer;
import jescript.preprocessor.lexer.LexerException;
import jescript.preprocessor.node.Start;
import jescript.preprocessor.node.Token;
import jescript.preprocessor.parser.Parser;
import jescript.preprocessor.parser.ParserException;

public class PreprocessorLexer extends Lexer {

	LinkedList<Token> tokens;
	Start ast;
	
	public PreprocessorLexer(PushbackReader in) throws ParserException, LexerException, IOException {
		super(null);
		Lexer l = new Lexer(in);
		Parser p = new Parser(l);
		Preprocessor preprocessor = new Preprocessor();
		ast = p.parse();
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
