package jescript.parser;

import jescript.analysis.DepthFirstAdapter;
import jescript.node.*;

public class ASTFix extends DepthFirstAdapter {
	@Override public void outAAltAtomExpr(AAltAtomExpr node) {
		PAltAtom atom = node.getAltAtom();
		TAtom atomTok = null;
		if (atom instanceof AFileAltAtom) {
			atomTok = new TAtom("file");
		} else if (atom instanceof AModuleAltAtom) {
			atomTok = new TAtom("module");
		} else if (atom instanceof AExportAltAtom) {
			atomTok = new TAtom("export");
		} else if (atom instanceof AImportAltAtom) {
			atomTok = new TAtom("import");
		} else if (atom instanceof ACompileAltAtom) {
			atomTok = new TAtom("compile");
		} else if (atom instanceof ARecordAltAtom) {
			atomTok = new TAtom("record");
		} else {
			throw new IllegalStateException("Unrecognized atom " + node.toString());
		}
		node.replaceBy(new AAtomExpr(atomTok));
	}
	@Override public void outAStringsExpr(AStringsExpr node) {
		TString s = node.getString();
		node.replaceBy(new AStringExpr(joinStrings(node.getExpr(), trimQuotes(s.getText()), s.getLine(), s.getPos())));
	}
	
	private TString joinStrings(PExpr next, String text, int line, int pos) {
		if (next instanceof AStringExpr) {
			AStringExpr last = (AStringExpr) next;
			return new TString('"' + trimQuotes(last.getString().getText()) + text + '"', line, pos);
		}
		AStringsExpr strings = (AStringsExpr) next;
		return joinStrings(strings.getExpr(), trimQuotes(strings.getString().getText()) + text, line, pos);
	}
	
	private String trimQuotes(String text) {
		return text.substring(1, text.length()-1);
	}
}
