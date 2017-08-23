@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Generator

import List;
import ConcreteSyntax;
//import domain::Syntax;
//import domain::SyntaxBase;
//import domain::rsd::Syntax;
//import domain::radt::Syntax;

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
	if (/(BoolExpr)`word` := bs)
		result += "lexical BoolWord = [A-Za-z]+ !\>\> [A-Za-z];\n";
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
str genSyntax((BoolExpr)`word`)
	= "BoolWord";
default str genSyntax(BoolExpr x)
{
	throw "Non-exhaustive pattern for <x>";
}

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
{
	throw "Non-exhaustive pattern for <x>";
}
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
{
	throw "Non-exhaustive pattern for <x>";
}

///////////////////////////////////////////////////////////////////
