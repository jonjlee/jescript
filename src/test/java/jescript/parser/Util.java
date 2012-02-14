package jescript.parser;

import static org.testng.Assert.*;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
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

	public static void assertASTEquals(Node start, Node expected) {
		Node root = ((Start) start).getPExpr();
		assertEquals(nodeToString(root), nodeToString(expected));
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
							setOut(n, n);
						}
					}
					@Override public void caseAImportAttrExpr(AImportAttrExpr node) { setOut(node, node.getModule().getText()); }
					public void caseARecordAttrExpr(ARecordAttrExpr node) { setOut(node, node.getType()); }
					public void caseARecFields(ARecFields node) {
						String ret = node.getName().getText();
						if (node.getValue() != null) {
							node.getValue().apply(this);
							if (getOut(node.getValue()) != null) { ret += " = " + getOut(node.getValue()); };
						}
						setOut(node, ret);
					}
					@Override public void caseAFunClause(AFunClause node) { setOut(node, node.getName()); }
					@Override public void caseAListExpr(AListExpr node) { if (node.getElts().size() == 0) { setOut(node, "[]"); }}
					@Override public void caseAFunArity(AFunArity node) { setOut(node, node.getName().getText() + "/" + node.getArity().getText()); }
				};
				String s = "";
				
				n.apply(nodeText);
				if (nodeText.getOut(n) != null) {
					s = "(" + nodeText.getOut(n).toString().trim() + ")";
				}
				return s;
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
	
	// AST construction convenience methods
	public static List<PExpr> none = Collections.unmodifiableList(new LinkedList<PExpr>());
	public static TAtom atomTok(String atom) {
		return new TAtom(atom);
	}
	public static TInteger intTok(int i) {
		return new TInteger(i + "");
	}
	public static TString strTok(String s) {
		return new TString('"' + s + '"');
	}
	public static PExpr atom(String atom) {
		return new AAtomExpr(atomTok(atom));
	}
	public static List<PExpr> atoms(String... atoms) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (String atom : atoms) {
			exprs.add(atom(atom));
		}
		return exprs;
	}
	public static PExpr integer(int num) {
		return new AIntegerExpr(intTok(num));
	}
	public static List<PExpr> integers(int... nums) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (int num : nums) {
			exprs.add(integer(num));
		}
		return exprs;
	}
	public static PExpr decimal(double num) {
		return new ADecimalExpr(new TDecimal(num + ""));
	}
	public static PExpr string(String s) {
		return new AStringExpr(strTok(s));
	}
	public static PExpr var(String v) {
		return new AVarExpr(new TVar(v));
	}
	public static List<PExpr> vars(String... vars) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (String v: vars) {
			exprs.add(var(v));
		}
		return exprs;
	}
}
