@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Generator

import List;
import domain::Syntax;
import domain::SyntaxBase;
import domain::rsd::Syntax;
import domain::radt::Syntax;

str genHeader(str name)
  = "@contributor{BOOL}
	'module <name>
	'
	'import ParseTree;
	'
	'";

///////////////////////////////////////////////////////////////////
str genStandard(Bindings bs)
{
	str result = "";
	if (/(RSDSimpleType)`word` := bs)
		result += "lexical BOOLWord = [A-Za-z]+ !\>\> [A-Za-z];\n";
	return result;
}

///////////////////////////////////////////////////////////////////
str genSD(str name, RascalSyntaxDef left, RascalAlgebraicDataType right)
{
	if ((RascalAlgebraicDataType)`.` := right)
		return "layout <name> = <genLex(left)>;\n";
	return "syntax <name> = <genSyntax(left)>;";
}

///////////////////////////////////////////////////////////////////
str genSyntax((RascalSyntaxDef)`star[<RascalSyntaxDef inner>]`)
	= "<genSyntax(inner)>*";
str genSyntax((RascalSyntaxDef)`plus[<RascalSyntaxDef inner>]`)
	= "<genSyntax(inner)>+";
str genSyntax((RascalSyntaxDef)`or[<{RascalSyntaxDef ","}+ inners>]`)
	= intercalate(" | ", [genSyntax(inner) | RascalSyntaxDef inner <- inners]);
str genSyntax((RascalSyntaxDef)`word`)
	= "BOOLWord";
default str genSyntax(RascalSyntaxDef x)
{
	throw "Non-exhaustive pattern for <x>";
}

///////////////////////////////////////////////////////////////////
str genLex((RascalSyntaxDef)`or[<{RascalSyntaxDef ","}+ inners>]`)
	= "[" + intercalate(" ", [genLex(inner) | RascalSyntaxDef inner <- inners]) + "]";
str genLex((RascalSyntaxDef)`word`)
	= "[A-Za-z]+ !\>\> [A-Za-z]";
str genLex((RascalSyntaxDef)`space`)
	= "\\ ";
str genLex((RascalSyntaxDef)`tab`)
	= "\\t";
str genLex((RascalSyntaxDef)`newline`)
	= "\\n";
default str genLex(RascalSyntaxDef x)
{
	throw "Non-exhaustive pattern for <x>";
}
///////////////////////////////////////////////////////////////////
str genADT(str name, (RascalAlgebraicDataType)`.`)
	= "";
default str genADT(str name, RascalAlgebraicDataType def)
	= "alias <name> = <genType(def)>;";

str genType((RascalAlgebraicDataType)`list[<RascalAlgebraicDataType inner>]`)
	= "list[<genType(inner)>]";
str genType((RascalAlgebraicDataType)`set[<RascalAlgebraicDataType inner>]`)
	= "set[<genType(inner)>]";
str genType((RascalAlgebraicDataType)`int`)
	= "int";
str genType((RascalAlgebraicDataType)`str`)
	= "str";
str genType((RascalAlgebraicDataType)`.`)
	= "";
default str genType(RascalAlgebraicDataType x)
{
	throw "Non-exhaustive pattern for <x>";
}

///////////////////////////////////////////////////////////////////
