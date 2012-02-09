package jescript.lexer;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import jescript.preprocessor.analysis.DepthFirstAdapter;
import jescript.preprocessor.node.*;

public class Preprocessor extends DepthFirstAdapter {

	LinkedList<Token> tokens = new LinkedList<Token>();
	Map<String, LinkedList<Token>> macros = new HashMap<String, LinkedList<Token>>();
	String macroName;

	public Preprocessor clear() {
		tokens = new LinkedList<Token>();
		return this;
	}

	public LinkedList<Token> getTokens() {
		return tokens;
	}

	private String getMacroName(Node n) {
		macroName = null;
		n.apply(this);
		String ret = macroName;
		macroName = null;
		return ret;
	}
	
	public void defaultCase(Node node) {
		if (node instanceof Token) {
			tokens.add((Token) node);
		}
	}
	@Override public void caseTAttrEnd(TAttrEnd node) {
		tokens.add(new TRparen(node.getLine(), node.getPos()));
		tokens.add(new TDot(node.getLine(), node.getPos()+1));
	}
	@Override public void caseAMacroDefStmt(AMacroDefStmt node) {
		String name = getMacroName(node.getName());
		LinkedList<Token> body = new LinkedList<Token>();
		Preprocessor tokenGetter = new Preprocessor();
		for (PMacroBodyTokens t : node.getBody()) {
			t.apply(tokenGetter.clear());
			body.addAll(tokenGetter.getTokens());
		}
		macros.put(name, body);
	}
	@Override public void caseAMacroApplyStmt(AMacroApplyStmt node) {
		String name = node.getName().getText().substring(1);
		if (macros.get(name) == null) {
			throw new RuntimeException("Macro " + name + " not defined.");
		}
		tokens.addAll(macros.get(name));
	}
	@Override public void caseALowerMacroName(ALowerMacroName node) { macroName = node.getAtom().getText(); }
	@Override public void caseAUpperMacroName(AUpperMacroName node) { macroName = node.getVar().getText(); }
	@Override public void caseAUnderscoreMacroName(AUnderscoreMacroName node) { macroName = "_"; }
}
