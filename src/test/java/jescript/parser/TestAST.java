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
	
	public void funArgs() {
		testFunArgs("\"A\"++\"B\"", new AListopExpr(string("A"), new AConcListop(), string("B")));
		testFunArgs("1+2", new AArithopExpr(integer(1), new APlusArithop(), integer(2)));
		testFunArgs("1*2", new AArithopExpr(integer(1), new ATimesArithop(), integer(2)));
		testFunArgs("{1}", new ATupleExpr(integers(1)));
		testFunArgs("#person{name=1}", new ARecMatchExpr(atomTok("person"), Arrays.asList((PRecFields) new ARecFields(atomTok("name"), integer(1)))));
		testFunArgs("[1|2]", new AListExpr(integers(1), new AIntegerExpr(intTok(2))));
		testFunArgs("[]", new AListExpr(none, null));
		testFunArgs("(ok)",  atoms("ok"));
	}
	
	public void invalidFunArgs() {
		testInvalidModule("f(X=2, Y=X*2) -> ok.");
	}
	
	public void funGuards() {
		testGuards("true", atom("true"));
		testGuards("true, true", atom("true"), atom("true"));
		testGuards("X<1, Y==0", 
				new ABoolopExpr(var("X"), new ALtBoolop(), integer(1)), 
				new ABoolopExpr(var("Y"), new AEqBoolop(), integer(0)));
		testGuards("X+1 == Y*2", 
				new ABoolopExpr(
						new AArithopExpr(var("X"), new APlusArithop(), integer(1)), 
						new AEqBoolop(), 
						new AArithopExpr(var("Y"), new ATimesArithop(), integer(2))));
		testGuards("-X", new APreopExpr(new ANegPreop(), var("X")));
		testGuards("is_integer(X)", new AApplyExpr(null, atom("is_integer"), vars("X")));
		testGuards("X#num.val==1", 
				new ABoolopExpr(
						new ARecReadExpr(var("X"), atomTok("num"), atomTok("val")),
						new AEqBoolop(),
						integer(1)));
		testGuards("X, (X)", var("X"), var("X"));
		testGuards("[1|2], []",
				new AListExpr(integers(1), integer(2)),
				new AListExpr(none, null));
		testGuards("{1}", new ATupleExpr(integers(1)));
	}

	public void invalidFunGuards() {
		testInvalidModule("f(X) when X=1 -> ok.");
		testInvalidModule("f(X) when m:f(1) -> ok.");
	}

	// AST construction convenience methods
	private TAtom atomTok(String atom) {
		return new TAtom(atom);
	}
	private TInteger intTok(int i) {
		return new TInteger(i + "");
	}
	private TString strTok(String s) {
		return new TString('"' + s + '"');
	}
	private PExpr atom(String atom) {
		return new AAtomExpr(atomTok(atom));
	}
	private List<PExpr> atoms(String... atoms) {
		List<PExpr> exprs = new LinkedList<PExpr>();
		for (String atom : atoms) {
			exprs.add(atom(atom));
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
	
	// Tester convenience methods
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
	private void testFunArgs(String args, List<PExpr> ast) {
		testModule("main(" + args + ") -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						ast,
						none,
						atoms("ok")))));
	}
	private void testFunArgs(String args, PExpr... ast) {
		testFunArgs(args, Arrays.asList(ast));
	}
	private void testGuards(String guards, PExpr... ast) {
		testModule("main() when " + guards + " -> ok.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("main"),
						none,
						Arrays.asList(ast),
						atoms("ok")))));
	}
	
	private void testInvalidInput(String input) { assertInvalid(parse(input)); }
	private void testInvalidModule(String input) { testInvalidInput("-module(m).\n" + input); }

	// Asserts
	private void assertASTEquals(Node start, Node expected) {
		Node root = ((Start) start).getPExpr();
		assertEquals(nodeToString(root), nodeToString(expected));
	}
	
	// Parse input into AST
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
