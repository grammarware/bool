@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Generator

import IO;
import List;
import ConcreteSyntax;

str genHeader(str name)
  = "@contributor{BOOL}
	'module <name>
	'
	'import ParseTree;
	'
	'";

///////////////////////////////////////////////////////////////////
str genStandard(BOOL bs)
{
	str result = "";
	if (/(NullaryOp)`word` := bs)
		result += "lexical BoolWord = [A-Za-z]+ !\>\> [A-Za-z];\n";
	if (/(NullaryOp)`int` := bs)
		result += "lexical BoolInt = [0-9]+ !\>\> [0-9];\n";
	return result;
}

///////////////////////////////////////////////////////////////////
str genSD(str name, BoolExpr left, BoolExpr right)
{
	if ((BoolExpr)`.` := right)
		return "layout <name> = <genLex(left)>;\n";
	return "syntax <name> = <genSyntax(left)>;";
}

///////////////////////////////////////////////////////////////////
str genSyntax((BoolExpr)`star[<BoolExpr inner>]`)
	= "<genSyntax(inner)>*";
str genSyntax((BoolExpr)`plus[<BoolExpr inner>]`)
	= "<genSyntax(inner)>+";
str genSyntax((BoolExpr)`or[<{BoolExpr ","}+ inners>]`)
	= intercalate(" | ", [genSyntax(inner) | BoolExpr inner <- inners]);
str genSyntax((BoolExpr)`seq[<{BoolExpr ","}+ inners>]`)
	= intercalate(" ", [genSyntax(inner) | BoolExpr inner <- inners]);
str genSyntax((BoolExpr)`<NullaryOp con>`)
	= genSyntaxSymbol(con);
str genSyntax((BoolExpr)`<NullaryOp con><NormalId name>`)
	= "<genSyntaxSymbol(con)> <name>";
default str genSyntax(BoolExpr x)
	= NonExhaustive("genSyntax", "<x>");

///////////////////////////////////////////////////////////////////
str genSyntaxSymbol((NullaryOp)`space`) = "\" \"";
str genSyntaxSymbol((NullaryOp)`tab`) = "\"\\t\"";
str genSyntaxSymbol((NullaryOp)`newline`) = "\"\\n\"";
str genSyntaxSymbol((NullaryOp)`comma`) = "\",\"";
str genSyntaxSymbol((NullaryOp)`int`) = "BoolInt";
str genSyntaxSymbol((NullaryOp)`str`) = "BoolStr";
str genSyntaxSymbol((NullaryOp)`word`) = "BoolWord";
str genSyntaxSymbol((NullaryOp)`method`) = "/*method*/";
str genSyntaxSymbol((NullaryOp)`.`) = " ";

default str genSyntaxSymbol(NullaryOp x)
	= NonExhaustive("genSyntaxSymbol", "<x>");
///////////////////////////////////////////////////////////////////
str genLex((BoolExpr)`or[<{BoolExpr ","}+ inners>]`)
	= "[" + intercalate(" ", [genLex(inner) | BoolExpr inner <- inners]) + "]";
str genLex((BoolExpr)`word`)
	= "[A-Za-z]+ !\>\> [A-Za-z]";
str genLex((BoolExpr)`space`)
	= "\\ ";
str genLex((BoolExpr)`tab`)
	= "\\t";
str genLex((BoolExpr)`newline`)
	= "\\n";
default str genLex(BoolExpr x)
	= NonExhaustive("genLex", "<x>");

///////////////////////////////////////////////////////////////////
str genADT(str name, (BoolExpr)`.`)
	= "";
default str genADT(str name, BoolExpr def)
	= "alias <name> = <genType(def)>;";

str genType((BoolExpr)`list[<BoolExpr inner>]`)
	= "list[<genType(inner)>]";
str genType((BoolExpr)`set[<BoolExpr inner>]`)
	= "set[<genType(inner)>]";
str genType((BoolExpr)`int`)
	= "int";
str genType((BoolExpr)`str`)
	= "str";
str genType((BoolExpr)`.`)
	= "";
default str genType(BoolExpr x)
	= NonExhaustive("genType", "<x>");

///////////////////////////////////////////////////////////////////

private str NonExhaustive(str f, str x)
{
	println("Non-exhaustive pattern in <f> for <x>");
	return "/*<x>*/";
}
