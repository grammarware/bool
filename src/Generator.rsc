@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Generator

import Prelude;
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
	return "syntax C<name> = <genSyntax(left)>;";
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
	= "[" + intercalate(" ", [unpack(genLex(inner)) | BoolExpr inner <- inners]) + "]";
str genLex((BoolExpr)`<NullaryOp con>`)
	= genLexSymbol(con);
str genLex((BoolExpr)`<NullaryOp con><NormalId name>`)
	= "<genLexSymbol(con)> <name>";
default str genLex(BoolExpr x)
	= NonExhaustive("genLex", "<x>");

str unpack(str x)
{
	if (startsWith(x,"[") && endsWith(x,"]"))
		return substring(x,1,size(x)-1);
	else
		return x;
}

///////////////////////////////////////////////////////////////////
str genLexSymbol((NullaryOp)`space`) = "[\\ ]";
str genLexSymbol((NullaryOp)`tab`) = "[\\t]";
str genLexSymbol((NullaryOp)`newline`) = "[\\n]";
str genLexSymbol((NullaryOp)`comma`) = "[,]";
str genLexSymbol((NullaryOp)`int`) = "[0-9]+ !\>\> [0-9]";
str genLexSymbol((NullaryOp)`str`) = "/*str*/";
str genLexSymbol((NullaryOp)`word`) = "[A-Za-z]+ !\>\> [A-Za-z]";
str genLexSymbol((NullaryOp)`method`) = "/*method*/";
str genLexSymbol((NullaryOp)`.`) = " ";

default str genLexSymbol(NullaryOp x)
	= NonExhaustive("genLexSymbol", "<x>");

///////////////////////////////////////////////////////////////////
str genADT(str name, (BoolExpr)`.`)
	= "";
default str genADT(str name, BoolExpr def)
	= "alias A<name> = <genType(def)>;";

str genType((BoolExpr)`class[<{BoolExpr ","}+ inners>]`)
	= "tuple[" + intercalate(", ", [genType(inner) | BoolExpr inner <- inners]) + "]";
str genType((BoolExpr)`list[<BoolExpr inner>]`)
	= "list[<genType(inner)>]";
str genType((BoolExpr)`set[<BoolExpr inner>]`)
	= "set[<genType(inner)>]";
str genType((BoolExpr)`<NullaryOp con>`)
	= genTypeSymbol(con);
str genType((BoolExpr)`<NullaryOp con><NormalId name>`)
	= "<genTypeSymbol(con)> <name>";
str genType((BoolExpr)`<UserId con>`)
	= "<con>";
str genType((BoolExpr)`<UserId con><NormalId name>`)
	= "<con> <name>";
default str genType(BoolExpr x)
	= NonExhaustive("genType", "<x>");

///////////////////////////////////////////////////////////////////
str genTypeSymbol((NullaryOp)`int`) = "int";
str genTypeSymbol((NullaryOp)`str`) = "str";
str genTypeSymbol((NullaryOp)`method`) = "value/*method*/";
str genTypeSymbol((NullaryOp)`.`) = " ";

default str genTypeSymbol(NullaryOp x)
	= NonExhaustive("genTypeSymbol", "<x>");

///////////////////////////////////////////////////////////////////
private str NonExhaustive(str f, str x)
{
	println("Non-exhaustive pattern in <f> for <x>");
	return "/*<x>*/";
}

///////////////////////////////////////////////////////////////////
str genMethods(BoolBind b)
{
	str result = "";
	pair = split(".", "<b.name>");
	//iprintln(b.right.con);
	str pars = "";
	
	
	result += "A<b.right.con> <pair[1]><pair[0]>(<intercalate(", ", [genType(x) | BoolExpr x <- b.left.inners])>)
			  '{
			  '	return new<b.right.con>(<intercalate(", ", ["<ba.expr>" | BoolAssignment ba <- b.right.inners])>);
			  '}";
	return result;
}