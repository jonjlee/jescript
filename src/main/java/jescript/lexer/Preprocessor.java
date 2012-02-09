package jescript.lexer;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import jescript.node.TDot;
import jescript.node.TRparen;
import jescript.node.Token;
import jescript.preprocessor.analysis.DepthFirstAdapter;
import jescript.preprocessor.node.ALowerMacroName;
import jescript.preprocessor.node.AMacroApplyTokens;
import jescript.preprocessor.node.AMacroDefTokens;
import jescript.preprocessor.node.AUnderscoreMacroName;
import jescript.preprocessor.node.AUpperMacroName;
import jescript.preprocessor.node.Node;
import jescript.preprocessor.node.PMacroBodyTokens;
import jescript.preprocessor.node.TAttrEnd;

public class Preprocessor extends DepthFirstAdapter {

	public static final String TOKEN_PACKAGE = "jescript.node.";

	LinkedList<Token> tokens = new LinkedList<Token>();
	Map<String, LinkedList<Token>> macros = new HashMap<String, LinkedList<Token>>();
	GetMacroName macroNameUtil = new GetMacroName();

	public Preprocessor clear() {
		tokens = new LinkedList<Token>();
		return this;
	}

	public LinkedList<Token> getTokens() {
		return tokens;
	}

	public static Token translateToken(jescript.preprocessor.node.Token token) {
		Token ret = null;
		
		// Replace jescript.preprocessor.node. with jescript.node, but retain the class name
		String className = token.getClass().toString();
		className = className.replaceFirst(".*\\.", TOKEN_PACKAGE);
		try {
			Class<?> tokenClass = Class.forName(className);
			Constructor<?> constructor;
			try {
				try {
					constructor = tokenClass.getConstructor(String.class, Integer.TYPE, Integer.TYPE);
					ret = (Token) constructor.newInstance(token.getText(), token.getLine(), token.getPos());
				} catch (SecurityException e) {
					throw new RuntimeException(e);
				} catch (NoSuchMethodException e) {
					try {
						constructor = tokenClass.getConstructor(Integer.TYPE, Integer.TYPE);
						ret = (Token) constructor.newInstance(token.getLine(), token.getPos());
					} catch (SecurityException e1) {
						throw new RuntimeException(e1);
					} catch (NoSuchMethodException e1) {
						throw new RuntimeException(e1);
					}
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException(e);
		}
		return ret;
	}

	public void defaultCase(Node node) {
		if (node instanceof jescript.preprocessor.node.Token) {
			tokens.add(translateToken((jescript.preprocessor.node.Token) node));
		}
	}
	@Override public void caseTAttrEnd(TAttrEnd node) {
		tokens.add(new TRparen(node.getLine(), node.getPos()));
		tokens.add(new TDot(node.getLine(), node.getPos()+1));
	}
	@Override public void caseAMacroDefTokens(AMacroDefTokens node) {
		String name = macroNameUtil.get(node.getName());
		LinkedList<Token> body = new LinkedList<Token>();
		Preprocessor tokenGetter = new Preprocessor();
		for (PMacroBodyTokens t : node.getBody()) {
			t.apply(tokenGetter.clear());
			body.addAll(tokenGetter.getTokens());
		}
		macros.put(name, body);
	}
	@Override public void caseAMacroApplyTokens(AMacroApplyTokens node) {
		String name = node.getName().getText().substring(1);
		if (macros.get(name) == null) {
			throw new RuntimeException("Macro " + name + " not defined.");
		}
		tokens.addAll(macros.get(name));
	}
}

class GetMacroName extends DepthFirstAdapter {
	private String macroName;
	public String get(jescript.preprocessor.node.Node n) {
		macroName = null;
		n.apply(this);
		String ret = macroName;
		macroName = null;
		return ret;
	}
	@Override public void caseALowerMacroName(ALowerMacroName node) { macroName = node.getAtom().getText(); }
	@Override public void caseAUpperMacroName(AUpperMacroName node) { macroName = node.getVar().getText(); }
	@Override public void caseAUnderscoreMacroName(AUnderscoreMacroName node) { macroName = "_"; }
}