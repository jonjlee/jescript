package jescript.parser;

import java.io.IOException;
import jescript.lexer.Lexer;
import jescript.lexer.LexerException;
import jescript.node.Start;

public class ErlangParser extends Parser {

	public ErlangParser(Lexer lexer) {
		super(lexer);
	}

	@Override public Start parse() throws ParserException, LexerException, IOException {
		Start s = super.parse();
		s.apply(new ASTFix());
		return s;
	}

}
