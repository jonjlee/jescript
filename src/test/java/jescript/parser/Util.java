package jescript.parser;

import static org.testng.Assert.*;
import jescript.analysis.AnalysisAdapter;
import jescript.analysis.DepthFirstAdapter;
import jescript.node.*;

public class Util {
	public static void assertValid(Node parseResult) {
		if (parseResult instanceof ParseFailed) {
			ParseFailed r = (ParseFailed) parseResult;
			fail(r.getMessage(), r.getException());
		}
	}
	
	public static void assertInvalid(Node parseResult) {
		assertInvalid(parseResult, -1, -1, -1, -1);
	}

	public static void assertInvalid(Node parseResult, int linestart, int lineend, int colstart, int colend) {
		if (!(parseResult instanceof ParseFailed)) {
			fail("Expected parse error but succeeded");
		}
		ParseFailed r = (ParseFailed) parseResult;
		if (!(r.getException() instanceof ParserException)) {
			fail("Expected a parse exception, but got " + r.getException().getMessage(), r.getException());
		}
		String[] pos = r.getException().getMessage().split("\\[|\\]|,");
		int line = Integer.parseInt(pos[1]), col = Integer.parseInt(pos[2]);
		assertTrue(linestart < 0 || (linestart <= line && lineend >= line), "\n    Error at unexpected position: " + r.getMessage());
		assertTrue(colstart < 0 || (colstart <= col && colend >= col),      "\n    Error at unexpected position: " + r.getMessage());
	}

	public static String nodeToString(Node s) {
		final StringBuilder ast = new StringBuilder();
		s.apply(new DepthFirstAdapter() {
			int indent = 0;
			void append(Node n) {
				StringBuilder s = new StringBuilder();
				for (int i = 0; i < indent; i++) {
					s.append("|  ");
				}
				s.append(n.getClass().toString().replaceFirst("class jescript.node.A?", "")).append(getText(n));
				ast.append(s).append("\n");
			}
			String getText(Node n) {
				AnalysisAdapter nodeText = new AnalysisAdapter() {
					public void defaultCase(Node n) {
						if (n instanceof AIntegerExpr || n instanceof ADecimalExpr || n instanceof AAtomExpr || n instanceof AAltAtomExpr || n instanceof AVarExpr || n instanceof ACharExpr || n instanceof AStringExpr || n instanceof AStringsExpr 
								|| n instanceof AFileAttrExpr || n instanceof AModuleAttrExpr || n instanceof ACompileAttrExpr || n instanceof ACustomAttrExpr) {
							setOut(n, n.toString());
						}
					}
					@Override public void caseAImportAttrExpr(AImportAttrExpr node) { setOut(node, node.getModule().getText()); }
					public void caseARecordAttrExpr(ARecordAttrExpr node) { setOut(node, node.getType().toString()); }
					public void caseARecFields(ARecFields node) {
						String ret = node.getName().getText();
						if (node.getValue() != null) {
							node.getValue().apply(this);
							if (getOut(node.getValue()) != null) { ret += " = " + getOut(node.getValue()); };
						}
						setOut(node, ret);
					}
					@Override public void caseAFunClause(AFunClause node) { setOut(node, node.getName().toString()); }
					@Override public void caseAListExpr(AListExpr node) { if (node.getElts().size() == 0) { setOut(node, "[]"); }}
					@Override public void caseAFunArity(AFunArity node) { setOut(node, node.getName().getText() + "/" + node.getArity().getText()); }
				};
				n.apply(nodeText);
				String s = (String) nodeText.getOut(n);
				if (s != null) {
					return "(" + s.trim() + ")";
				}
				return "";
			}
			@Override public void defaultIn(Node node) {
				append(node);
				indent++;
			}
			@Override public void defaultOut(Node node) {
				indent--;
			}
		});
		return ast.toString();
	}
}
