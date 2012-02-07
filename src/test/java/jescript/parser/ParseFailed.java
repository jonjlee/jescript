package jescript.parser;

import jescript.node.Switch;
import jescript.node.Token;

class ParseFailed extends Token {
	public String input;
	public Exception e;
	public ParseFailed(String input, Exception e) {
		this.input = input;
		this.e = e;
	}
	public String getMessage() {
		String[] pos = e.getMessage().split("\\[|\\]|,");
		int row = -1, col = -1;
		if (e instanceof ParserException) {
			row = Integer.parseInt(pos[1]);
			col = Integer.parseInt(pos[2]);
		}

		String indent = "       ";
		StringBuilder s = new StringBuilder("\n").append(indent).append(e.getMessage()).append(":\n");
		s.append(indent).append("1   .    10   .    20   .    30   .    40   .    50   .    60   .    70   .\n");
		s.append(indent);
		for (int i = 0; i < col-1; i++) {
			s.append(" ");
		}
		s.append("v\n");

		String[] lines = input.split("\n");
		for (int i = 0; i < lines.length; i++) {
			s.append(indent.substring(3))
				.append(i+1).append((i == row-1) ? ">" : ":").append(" ")
				.append(lines[i]).append('\n');
		}
		return s.toString();
	}
	@Override public void apply(Switch sw) { throw new UnsupportedOperationException(); }
	@Override public Object clone() { throw new UnsupportedOperationException(); }
}