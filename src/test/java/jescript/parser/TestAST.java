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
								Arrays.asList((PExpr) new AUniversalPatternExpr()),
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
	
	public void exprs() {
		testModule("f() -> a, a.",
				new AFunExpr(Arrays.asList((PFunClause) new AFunClause(
						atomTok("f"),
						none,
						none,
						atoms("a", "a")))));
		testExpr("a", atom("a"));
		testExpr("V", var("V"));
		testExpr("{1,2}", new ATupleExpr(integers(1,2)));
		testExpr("[1,2|3]", new AListExpr(integers(1,2), integer(3)));
		testExpr("[X || X <- [1,2,3], X>=2]", 
				new AComprehensionExpr(
						var("X"), 
						Arrays.asList(
								(PExpr) new AGeneratorExpr(var("X"), new AListExpr(integers(1,2,3), null)),
								(PExpr) new ABoolopExpr(var("X"), new AGeBoolop(), integer(2)))));
		testExpr("begin x, 1 end", new ABlockExpr(Arrays.asList(atom("x"), integer(1))));
		testExpr("if X -> x; true -> err end", 
			new AIfExpr(Arrays.asList(
					(PIfClause) new AIfClause(vars("X"), atoms("x")),
					(PIfClause) new AIfClause(atoms("true"), atoms("err"))
					)));
		testExpr("case X of Y when Y =/= 1 -> X; _ -> err end",
				new ACaseExpr(var("X"), Arrays.asList(
						(PCaseClause) new ACaseClause(
								var("Y"), 
								Arrays.asList((PExpr) new ABoolopExpr(var("Y"), new ANequivBoolop(), integer(1))),
								vars("X")),
						(PCaseClause) new ACaseClause(
								new AUniversalPatternExpr(),
								none,
								atoms("err")))));
		testExpr("receive {X} when X == 2 -> 2; {X} -> 3 after 1000 -> err end",
				new AReceiveExpr(Arrays.asList(
						(PCaseClause) new ACaseClause(
								new ATupleExpr(vars("X")),
								Arrays.asList((PExpr) new ABoolopExpr(var("X"), new AEqBoolop(), integer(2))),
								integers(2)),
						(PCaseClause) new ACaseClause(
								new ATupleExpr(vars("X")),
								none,
								integers(3))),
						integer(1000),
						atoms("err")));
		testExpr("fun m:f/1", new AFunRefExpr(atomTok("m"), atomTok("f"), intTok(1)));
		testExpr("fun (X) when X == 2 -> 2; (X) -> err end",
				new AFunExpr(Arrays.asList(
						(PFunClause) new AFunClause(
								null,
								vars("X"), 
								Arrays.asList((PExpr) new ABoolopExpr(var("X"), new AEqBoolop(), integer(2))),
								integers(2)),
						(PFunClause) new AFunClause(null, vars("X"), none, atoms("err")))));
		testExpr("(x)", atom("x"));
		
		testExpr("X=1", new AMatchExpr(var("X"), integer(1)));
		testExpr("dest ! a", new ASendExpr(atom("dest"), atom("a")));
		testExpr("X >= 1.0", new ABoolopExpr(var("X"), new AGeBoolop(), decimal(1.0)));
		testExpr("X =:= Y", new ABoolopExpr(var("X"), new AEquivBoolop(), var("Y")));
		testExpr("[65] ++ \"A\"", new AListopExpr(new AListExpr(integers(65), null),new AConcListop(), string("A")));
		testExpr("X+1", new AArithopExpr(var("X"), new APlusArithop(), integer(1)));
		testExpr("X*2", new AArithopExpr(var("X"), new ATimesArithop(), integer(2)));
		testExpr("-X", new APreopExpr(new ANegPreop(), var("X")));
		testExpr("X#rec.f", new ARecReadExpr(var("X"), atomTok("rec"), atomTok("f")));
		testExpr("X#rec{f=1}",
				new ARecUpdateExpr(
						var("X"),
						atomTok("rec"), 
						Arrays.asList((PRecUpdateFields) new ARecUpdateFields(atomTok("f"), integer(1)))));
		testExpr("f(x)", new AApplyExpr(null, atom("f"), atoms("x")));
		testExpr("m:f(x)", new AApplyExpr(atom("m"), atom("f"), atoms("x")));
	}
	
	public void exprPrecedence() {
		testExpr("dest ! (X=1)", new ASendExpr(atom("dest"), new AMatchExpr(var("X"), integer(1))));
		testExpr("X=<Y = 1", new AMatchExpr(new ABoolopExpr(var("X"), new ALeBoolop(), var("Y")), integer(1)));
		testExpr("X = [1,2]--[1]", 
				new AMatchExpr(
						var("X"), 
						new AListopExpr(
								new AListExpr(integers(1,2), null), 
								new ADelListop(), 
								new AListExpr(integers(1), null))));
		testExpr("1 bsl 2 * -3 / (4 + +5)",
				new AArithopExpr(
						integer(1), 
						new ABslArithop(), 
						new AArithopExpr(
								new AArithopExpr(
										integer(2),
										new ATimesArithop(),
										new APreopExpr(new ANegPreop(), integer(3))),
								new ADivideArithop(),
								new AArithopExpr(
										integer(4),
										new APlusArithop(),
										new APreopExpr(new APosPreop(), integer(5))))));
	}
	
	public void exprAssociativity() {
		testExpr("X=Y=1", new AMatchExpr(var("X"), new AMatchExpr(var("Y"), integer(1))));
		testExpr("X+Y+1", new AArithopExpr(new AArithopExpr(var("X"), new APlusArithop(), var("Y")), new APlusArithop(), integer(1)));
		testExpr("X div Y div 1", new AArithopExpr(new AArithopExpr(var("X"), new ADivArithop(), var("Y")), new ADivArithop(), integer(1)));
	}
	
	public void invalidExpr() {
		testInvalidExpr("X > Y > Z");
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
	private PExpr decimal(double num) {
		return new ADecimalExpr(new TDecimal(num + ""));
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
	private void testExpr(String input, PExpr ast) {
		testModule("f() -> " + input + ".",
				new AFunExpr(Arrays.asList((PFunClause) 
						new AFunClause(atomTok("f"), none, none, Arrays.asList(ast)))));
	}
	
	private void testInvalidInput(String input) { assertInvalid(parse(input)); }
	private void testInvalidModule(String input) { testInvalidInput("-module(m).\n" + input); }
	private void testInvalidExpr(String input) { testInvalidInput("-module(m).\nf() -> " + input + "."); }

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
