package jescript.lexer;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jescript.node.TDot;
import jescript.node.TRparen;
import jescript.node.Token;
import jescript.preprocessor.analysis.DepthFirstAdapter;
import jescript.preprocessor.node.*;

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
		if (token == null) { return null; }

		// Replace jescript.preprocessor.node. with jescript.node, but retain the class name
		String className = token.getClass().toString();
		className = className.replaceFirst(".*\\.", TOKEN_PACKAGE);
		try {
			Class<?> tokenClass = Class.forName(className);
			Constructor<?> constructor;
			try {
				// Token constructors come in two flavors, Token(text, line, pos) and Token(line,pos)
				constructor = tokenClass.getConstructor(String.class, Integer.TYPE, Integer.TYPE);
				ret = (Token) constructor.newInstance(token.getText(), token.getLine(), token.getPos());
			} catch (NoSuchMethodException e) {
				constructor = tokenClass.getConstructor(Integer.TYPE, Integer.TYPE);
				ret = (Token) constructor.newInstance(token.getLine(), token.getPos());
			}
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException(e);
		} catch (SecurityException e) {
			throw new RuntimeException(e);
		} catch (NoSuchMethodException e) {
			throw new RuntimeException(e);
		} catch (IllegalArgumentException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		} catch (InvocationTargetException e) {
			throw new RuntimeException(e);
		}
		return ret;
	}

	public void defaultCase(Node node) {
		if (node instanceof jescript.preprocessor.node.Token) {
			tokens.add(translateToken((jescript.preprocessor.node.Token) node));
		}
	}
	
	// Special preprocessor token ').' should be split apart into rparen and dot
	@Override public void caseTAttrEnd(TAttrEnd node) {
		tokens.add(new TRparen(node.getLine(), node.getPos()));
		tokens.add(new TDot(node.getLine(), node.getPos()+1));
	}
	
	// Store macro definition in table 
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

	// Constant (non-function) macro replacement - emit tokens from lookup table   
	@Override public void caseAMacroApplyTokens(AMacroApplyTokens node) {
		String name = macroNameUtil.get(node);
		if (macros.get(name) == null) {
			throw new RuntimeException("Macro " + name + " not defined.");
		}
		tokens.addAll(macros.get(name));
	}

	// Function macro replacement - lookup macro, do arg substitution, and emit tokens   
	@Override public void caseAMacroFunApplyTokens(AMacroFunApplyTokens node) {
		String name = macroNameUtil.get(node);
		if (macros.get(name) == null) {
			throw new RuntimeException("Macro " + name + " not defined.");
		}
		tokens.addAll(macros.get(name));
		
		// This node could end in the ').' token. The paren is part of the macro
		// application, so replace the token with a single '.'. 
		Node end = node.getMacroFunApplyEnd();
		if (end instanceof AStmtMacroFunApplyEnd) {
			jescript.preprocessor.node.Token t = ((AStmtMacroFunApplyEnd) end).getAttrEnd();
			tokens.add(new TDot(t.getLine(), t.getPos()+1));
		}
	}
}

// Extract the name from macro definition or application node
class GetMacroName extends DepthFirstAdapter {
	// Matches ?macro and ?macro(x,y) forms: '?' [_, @, digit lowercase, uppercase]+
	private Pattern macroNamePattern = Pattern.compile("\\?([_@0-9a-zA-Z\\xC0-\\xD6\\xD8-\\xF6\\xF8-\\xFF]+)");
	private String macroName;
	public String get(jescript.preprocessor.node.Node n) {
		macroName = null;
		n.apply(this);
		String ret = macroName;
		macroName = null;
		return ret;
	}
	private String match(String s) {
		Matcher m = macroNamePattern.matcher(s);
		return m.find() ? m.group(1) : null;
	}
	@Override public void caseALowerMacroName(ALowerMacroName node) { macroName = node.getAtom().getText(); }
	@Override public void caseAUpperMacroName(AUpperMacroName node) { macroName = node.getVar().getText(); }
	@Override public void caseAUnderscoreMacroName(AUnderscoreMacroName node) { macroName = "_"; }
	@Override public void caseAMacroApplyTokens(AMacroApplyTokens node) { macroName = match(node.getName().getText()); }
	@Override public void caseAMacroFunApplyTokens(AMacroFunApplyTokens node) { macroName = match(node.getName().getText()); }
}