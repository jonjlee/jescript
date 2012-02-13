package jescript.parser;

import static jescript.parser.Util.*;
import static org.testng.Assert.*;
import java.io.PushbackReader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import jescript.lexer.Lexer;
import jescript.node.*;
import org.testng.annotations.Test;
import org.testng.log4testng.Logger;

@Test
public class TestAST {

	private static Logger LOG = Logger.getLogger(TestAST.class);
	private static List<PExpr> none = Collections.unmodifiableList(new LinkedList<PExpr>());

	public void emptyModule() {
		testInput("-module(m).", new AModuleExpr(Arrays.asList((PExpr) new AModuleAttrExpr(atomTok("m")))));
	}
	
	public void attributes() {
		testInput(
				"-file(\"m\",2).\n" +
				"-module(m).\n" +
				"-import(io, [format/2]).\n" +
				"-export([main/1]).\n" +
				"-custom([]).\n" +
				"-record(r, {x}).",
				new AModuleExpr(Arrays.asList(
						(PExpr) new AFileAttrExpr(strTok("m"), intTok(2)),
						(PExpr) new AModuleAttrExpr(atomTok("m")),
						(PExpr) new AImportAttrExpr(atomTok("io"), Arrays.asList((PFunArity) new AFunArity(atomTok("format"), intTok(2)))),
						(PExpr) new AExportAttrExpr(Arrays.asList((PFunArity) new AFunArity(atomTok("main"), intTok(1)))),
						(PExpr) new ACustomAttrExpr(new AListExpr()),
						(PExpr) new ARecordAttrExpr(atomTok("r"), Arrays.asList((PRecFields) new ARecFields(atomTok("x"), null))))
						));
	}
	
	public void simpleFuns() {
		testModule("main() -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						none,
						none,
						atoms("ok")))));
		testModule("main(Args) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						vars("Args"),
						none,
						atoms("ok")))));
		testModule("main(Args) when true -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						vars("Args"),
						atoms("true"),
						atoms("ok")))));
		testModule("main(Args) when true -> ok; main(_) when true -> err.",
				new AFunExpr(Arrays.asList(
						(PFunClause) new AFunClause(
								atomTok("main"),
								vars("Args"),
								atoms("true"),
								atoms("ok")),
						(PFunClause) new AFunClause(
								atomTok("main"),
								Arrays.asList((PExpr) new AUniversalExpr()),
								atoms("true"),
								atoms("err"))
						)));
	}
	
	public void funArgExprs() {
		testModule("main(X=1) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AMatchExpr(var("X"), integer(1))),
						none,
						atoms("ok")))));
		testModule("main(\"A\"++\"B\") -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AListopExpr(string("A"), new AConcListop(), string("B"))),
						none,
						atoms("ok")))));
		testModule("main(1+2) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AArithopExpr(integer(1), new APlusArithop(), integer(2))),
						none,
						atoms("ok")))));
		testModule("main(1*2) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AArithopExpr(integer(1), new ATimesArithop(), integer(2))),
						none,
						atoms("ok")))));
		testModule("main(-1) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new APreopExpr(new ANegPreop(), integer(1))),
						none,
						atoms("ok")))));
		testModule("main({1}) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new ATupleExpr(integers(1))),
						none,
						atoms("ok")))));
		testModule("main(#person{name=1}) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new ARecMatchExpr(atomTok("person"), Arrays.asList((PRecFields) new ARecFields(atomTok("name"), integer(1))))),
						none,
						atoms("ok")))));
		testModule("main([1|2]) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AListExpr(integers(1), new AIntegerExpr(intTok(2)))),
						none,
						atoms("ok")))));
		testModule("main([]) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						Arrays.asList((PExpr) new AListExpr(none, null)),
						none,
						atoms("ok")))));
		testModule("main((ok)) -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						atoms("ok"),
						none,
						atoms("ok")))));
	}

	private TAtom atomTok(String atom) {
		return new TAtom(atom);
	}
	private TInteger intTok(int i) {
		return new TInteger(i + "");
	}
	private TString strTok(String s) {
		return new TString('"' + s + '"');
	}
	private List<PExpr> atoms(String... atoms) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (String atom : atoms) {
			exprs.add(new AAtomExpr(atomTok(atom)));
		}
		return exprs;
	}
	private PExpr integer(int num) {
		return new AIntegerExpr(intTok(num));
	}
	private List<PExpr> integers(int... nums) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (int num : nums) {
			exprs.add(integer(num));
		}
		return exprs;
	}
	private PExpr string(String s) {
		return new AStringExpr(strTok(s));
	}
	private PExpr var(String v) {
		return new AVarExpr(new TVar(v));
	}
	private List<PExpr> vars(String... vars) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (String v: vars) {
			exprs.add(var(v));
		}
		return exprs;
	}
	
	private void testInput(String input, Node n) {
		Node s = parse(input);
		assertValid(s);
		assertASTEquals(s, n);
	}
	private void testModule(String input, List<? extends PExpr> n) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		exprs.add(new AModuleAttrExpr(atomTok("m")));
		exprs.addAll(n);
		testInput("-module(m).\n" + input, new AModuleExpr(exprs)); }
	private void testModule(String input, PExpr n) { 
		testModule(input, Arrays.asList(n)); 
	}

	private void assertASTEquals(Node start, Node expected) {
		Node root = ((Start) start).getPExpr();
		assertEquals(nodeToString(root), nodeToString(expected));
	}
	
	private Node parse(String input) {
		try {
			Lexer l = new Lexer(new PushbackReader(new StringReader(input), 1024));
			Parser p = new Parser(l);
			Start s = p.parse();
			LOG.debug("\n" + input + " = \n" + nodeToString(s));
			return s;
		} catch (Exception e) {
			return new ParseFailed(input, e);
		}
	}
	
}
