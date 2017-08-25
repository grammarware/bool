@contributor{Vadim Zaytsev - vadim@grammarware.net}
module internal::Generator

import Prelude;
import internal::ConcreteSyntax;

list[str] AllClasses = [];
map[str,list[str]] AllMethods = ();

public void TransformBoolToRascal(str modname, loc infile, loc outfile)
{
	T = parse(#start[BOOL], infile).top;
	
	// Remember all classes
	AllClasses = ["<name>" | /(BoolBind)`<UserId name>:= <BoolExpr _> ~ class[<{BoolExpr ","}+ _>]` := T];
	// Collect methods' signatures
	AllMethods = CollectMethodSignatures(T);
	
	// Generate Rascal header
	str text = Header(modname)
	// Add "standard" library nonterminals
			 + StandardDefinitions(T)
	// Complete the concrete syntax part
			 + SyntaxDefinitions(T)
	// Add the abstract syntax part
			 + AliasDefinitions(T)
	// Concrete to abstract mapping
			 + ConcreteToAbstract(T)
	// Methods as separate functions
			 + StandaloneMethods(T)
	// Compose clusters of methods
			 + MethodClusters(T);
	// Serialise into the file
	writeFile(outfile, text);
}

private str SyntaxDefinitions(BOOL T)
	= intercalate("\n", [genSD("<b.name>", b.left, b.right) | /BoolBind b := T, !contains("<b.name>", ".")])
	+ "\n";

private str AliasDefinitions(BOOL T)
	= intercalate("\n", [genADT("<b.name>", b.left, b.right, T) | /BoolBind b := T, !contains("<b.name>", ".")])
	+ "\n";

private str ConcreteToAbstract(BOOL T)
{
	str text = "";
	for(/BoolBind b := T,
		!contains("<b.name>", "."),
		(BoolExpr)`.` !:= b.right,
		(BoolExpr)`<UserId _>` !:= b.left)
	{
		str c = "<b.name>";
		text += "
				'A<c> implode<c>(C<c> T)
				'	= <genImplosion(b, T)>;
				'A<c> implode<c>(str input) = implode<c>(parse(#C<c>, input));
				'";
	}
	return text;
}

private str StandaloneMethods(BOOL T)
	= intercalate("\n", [genSeparateMethod(b) | str c <- AllClasses, /BoolBind b := T, startsWith("<b.name>", c+".")])
	+ "\n";

private map[str,list[str]] CollectMethodSignatures(BOOL T)
{
	map[str,list[str]] methods = ();
	for(/BoolBind b := T,
		(BoolExpr)`fun[<{BoolExpr ","}+ inners>]` := b.left)
		methods["<b.name>"]
			= ["<b.right.con>"]
			+ ["<inner.con>" | BoolExpr inner <- inners];
	// Fake inheritance
	for(/BoolBind b := T,
		(BoolExpr)`<UserId con>` := b.left,
		/BoolBind b2 := T,
		"<b2.name>" == "<con>",
		(BoolExpr)`class[<{BoolExpr ","}+ inners>]` := b2.right,
		(BoolExpr)`method <NormalId name>` <- inners)
			methods["<b.name>.<name>"] = methods["<con>.<name>"];
	return methods;
}

private str MethodClusters(BOOL T)
{
	// NB: suboptimal code, needs rewriting
	str text = "";
	list[str] processed = [];
	for(str c <- AllClasses, /BoolBind b1 := T, startsWith("<b1.name>", c+"."))
	{
		if (c in processed) continue;
		text += "
				'public I<c> <c> = \<
				'	<intercalate(",\n", [genMethods(b) | /BoolBind b := T, startsWith("<b.name>", c+".")])>";
		list[BoolBind] mybind = [b3 | /BoolBind b3 := T, "<b3.name>" == c];
		if (size(mybind)!=1)
			throw "<c> must be unique!";
		if ((BoolExpr)`<UserId con>` := mybind[0].left)
		{
			text += ",
					'	<intercalate(",\n", [genMethods(b) | /BoolBind b := T, startsWith("<b.name>", "<con>.")])>";			
		}
		if (endsWith(trim(text),","))
			text = trim(text)[..size(trim(text))-1];
		text += "
				'\>;
				'";
		processed += c;
	}
	return text;
}

///////////////////////////////////////////////////////////////////
private str Header(str name)
  = "@contributor{BOOL}
	'module <name>
	'
	'import Prelude;
	'
	'";

///////////////////////////////////////////////////////////////////
private str StandardDefinitions(BOOL bs)
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
		return "layout <name> = <genLex(left)>* !\>\> <genLex(left)>;\n";
	return "syntax C<name> = <genSyntax(left)>;";
}

///////////////////////////////////////////////////////////////////
str genSyntax((BoolExpr)`star[<BoolExpr inner>]`)
	= "<genSyntax(inner)>*";
str genSyntax((BoolExpr)`plus[<BoolExpr inner>]`)
	= "<genSyntax(inner)>+ <toLowerCase(genSyntax(inner))>";
str genSyntax((BoolExpr)`or[<{BoolExpr ","}+ inners>]`)
	= intercalate(" | ", [genSyntax(inner) | BoolExpr inner <- inners]);
str genSyntax((BoolExpr)`seq[<{BoolExpr ","}+ inners>]`)
	= intercalate(" ", [genSyntax(inner) | BoolExpr inner <- inners]);
str genSyntax((BoolExpr)`<NullaryOp con>`)
	= genSyntaxSymbol(con);
str genSyntax((BoolExpr)`<NullaryOp con><NormalId name>`)
	= "<genSyntaxSymbol(con)> <name>";
str genSyntax((BoolExpr)`<UserId con>`)
	= "C<con>";
str genSyntax((BoolExpr)`<UserId con><NormalId name>`)
	= "C<con> <name>";
default str genSyntax(BoolExpr x)
	= NonExhaustive("genSyntax", "<x>");

///////////////////////////////////////////////////////////////////
str genSyntaxSymbol((NullaryOp)`space`) = "\" \"";
str genSyntaxSymbol((NullaryOp)`tab`) = "\"\\t\"";
str genSyntaxSymbol((NullaryOp)`newline`) = "\"\\n\"";
str genSyntaxSymbol((NullaryOp)`comma`) = "\",\"";
str genSyntaxSymbol((NullaryOp)`colon`) = "\":\"";
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
str genLexSymbol((NullaryOp)`colon`) = "[:]";
str genLexSymbol((NullaryOp)`int`) = "[0-9]+ !\>\> [0-9]";
str genLexSymbol((NullaryOp)`str`) = "/*str*/";
str genLexSymbol((NullaryOp)`word`) = "[A-Za-z]+ !\>\> [A-Za-z]";
str genLexSymbol((NullaryOp)`method`) = "/*method*/";
str genLexSymbol((NullaryOp)`.`) = " ";

default str genLexSymbol(NullaryOp x)
	= NonExhaustive("genLexSymbol", "<x>");

///////////////////////////////////////////////////////////////////
str genADT(str name, BoolExpr left, BoolExpr def, BOOL allbinds)
{
	if ((BoolExpr)`.` := def)
		return "";
	// TODO should be different for classes
	if ("<def.con>" == "record" || "<def.con>" == "class")
	{
		list[BoolExpr] fields = [], functs = [];
		for (BoolExpr x <- def.inners)
			if ("<x.con>" == "method")
				functs += x;
			else
				fields += x;
		if ((BoolExpr)`<UserId con>` := left)
		{
			list[BoolBind] mybinds = [b | /BoolBind b := allbinds, "<b.name>"=="<con>"];
			if (size(mybinds)!=1)
				throw "<con> must be unique!";
			for (BoolExpr x <- mybinds[0].right.inners)
				if ("<x.con>" == "method")
					functs += x;
				//else
				//	fields += x;
		}
		return
			"<if(!isEmpty(fields)){>alias A<name> = tuple[<intercalate(", ", [ToName(genType(f, name)) | BoolExpr f <- fields])>];
			'<}>
			'<if(!isEmpty(functs)){>alias I<name> = tuple[<intercalate(", ", [genType(f, name) | BoolExpr f <- functs])>];
			'<}>
	  		'<if(!isEmpty(fields)){><genConstructor(def, name)><}>";
	}
	else
		return "alias A<name> = <genType(def, "<name>")>;";
}

str genConstructor((BoolExpr)`class[<{BoolExpr ","}+ inners>]`, str super)
	= NewFromInners(super, inners);

str genConstructor((BoolExpr)`record[<{BoolExpr ","}+ inners>]`, str super)
	= NewFromInners(super, inners);
	
private str NewFromInners(str super, {BoolExpr ","}+ inners)
{
	list[BoolExpr] fields = [inner | BoolExpr inner <- inners, (BoolExpr)`method <NormalId _>` !:= inner];
	return  "A<super> new<super>(<intercalate(", ", [ToName(genType(field, super)) | field <- fields])>)
	  		'	= \< <intercalate(", ", ["<f.name>" | f <- fields])> \>;"; 
}

default str genConstructor(BoolExpr e, str super)
	= "// <e>";

///////////////////////////////////////////////////////////////////

str genType((BoolExpr)`record[<{BoolExpr ","}+ inners>]`, str super)
	= "tuple[" + intercalate(", ", [genType(inner, super) | BoolExpr inner <- inners]) + "]";
// TODO: change class to an ADT!
str genType((BoolExpr)`class[<{BoolExpr ","}+ inners>]`, str super)
	= "tuple[" + intercalate(", ", [genType(inner, super) | BoolExpr inner <- inners]) + "]";
str genType((BoolExpr)`list[<BoolExpr inner>]`, str super)
	= "list[<genType(inner, super)>]";
str genType((BoolExpr)`set[<BoolExpr inner>]`, str super)
	= "set[<genType(inner, super)>]";
str genType((BoolExpr)`<NullaryOp con>`, str super)
	= genTypeSymbol(con);
str genType((BoolExpr)`method <NormalId name>`, str super)
{
	list[str] args = AllMethods["<super>.<name>"];
	return "<ToName(args[0])>(<intercalate(", ", [ToName(a) | a <- args[1..]])>) <name>";
}
str genType((BoolExpr)`<NullaryOp con><NormalId name>`, str super)
	= "<genTypeSymbol(con)> <name>";
str genType((BoolExpr)`<UserId con>`, _)
	= "<con>";
str genType((BoolExpr)`<UserId con><NormalId name>`, _)
	= "<con> <name>";
default str genType(BoolExpr x, _)
	= NonExhaustive("genType", "<x>");

///////////////////////////////////////////////////////////////////
str genTypeSymbol((NullaryOp)`int`) = "int";
str genTypeSymbol((NullaryOp)`str`) = "str";
str genTypeSymbol((NullaryOp)`method`) = "value/*method*/";
str genTypeSymbol((NullaryOp)`.`) = " ";

default str genTypeSymbol(NullaryOp x)
	= NonExhaustive("genTypeSymbol", "<x>");

///////////////////////////////////////////////////////////////////
str genMethods(BoolBind b)
	= "A<b.right.con> (<intercalate(", ", [ToName(genType(x, "<b.right.con>")) | BoolExpr x <- b.left.inners])>) { return new<b.right.con>(<intercalate(", ", ["<ba.expr>" | BoolAssignment ba <- b.right.inners])>);}";

str genSeparateMethod(BoolBind b)
	= "A<b.right.con> <ReverseDot("<b.name>")> (<intercalate(", ", [ToName(genType(x, "<b.right.con>")) | BoolExpr x <- b.left.inners])>) { return new<b.right.con>(<intercalate(", ", ["<ba.expr>" | BoolAssignment ba <- b.right.inners])>);}";

///////////////////////////////////////////////////////////////////
str genImplosion(BoolBind b, BOOL allbinds)
	= genImplodePair(b.left, b.right, allbinds);

str genImplodePair(BoolExpr bl, BoolExpr br, BOOL allbinds)	
	= genImplodeAny("<br.con>", bl, br, allbinds);

str genImplodeAny("list", BoolExpr bl, BoolExpr br, BOOL allbinds)
{
	if ("<bl.con>" in ["plus", "star"])
		return "[ <replaceAll(genImplodePair(bl.inner, br.inner, allbinds), "T.<Expr2Name(bl.inner,br.inner)>", "element")> | <genSyntax(bl.inner)> element \<- T.<Expr2Name(bl.inner,br.inner)>]";
	return NonExhaustive("genImplodeAny", "<bl.con> to list");
}

// TODO should be different for classes
str genImplodeAny("class", BoolExpr bl, BoolExpr br, BOOL allbinds)
	= genImplodeAny("record", bl, br, allbinds);

str genImplodeAny("record", BoolExpr bl, BoolExpr br, BOOL allbinds)
{
	// TODO: name-based matching, not position-based
	list[BoolExpr] lefts, rights;
	if ((BoolExpr)`<UserId con>` := bl)
	{
		list[BoolBind] nb = [b | /BoolBind b := allbinds, b.name == con];
		if (size(nb)!=1)
			throw "<con> must be unique!";
		if ((BoolExpr)`<UserId con2>` := nb[0].left)
		{
			list[BoolBind] nb2 = [b | /BoolBind b := allbinds, b.name == con2];
			if (size(nb2)!=1)
				throw "<con2> must be unique!";
			lefts = [e | BoolExpr e <- nb2[0].left.inners, "<e.con>" notin ["space", "tab", "newline", "comma", "colon"]];
			rights = [e | BoolExpr e <- nb[0].right.inners, "<e.con>" != "method"]
				   + [e | BoolExpr e <- nb2[0].right.inners, "<e.con>" != "method"];
		}
		else
		{
			lefts = [e | BoolExpr e <- nb[0].left.inners, "<e.con>" notin ["space", "tab", "newline", "comma", "colon"]];
			rights = [e | BoolExpr e <- nb[0].right.inners, "<e.con>" != "method"];
		}
	}
	else
	{	
		lefts = [e | BoolExpr e <- bl.inners, "<e.con>" notin ["space", "tab", "newline", "comma", "colon"]];
		rights = [e | BoolExpr e <- br.inners, "<e.con>" != "method"];
	}
	return "\< <intercalate(", ", [genImplodePair(lefts[i], rights[i], allbinds) | int i <- [0..size(lefts)]])> \>";
}

str genImplodeAny("str", BoolExpr l, BoolExpr r, BOOL allbinds)
	= "\"\<T.<Expr2Name(l,r)>\>\"";

str genImplodeAny("int", BoolExpr l, BoolExpr r, BOOL allbinds)
	= "toInt(\"\<T.<Expr2Name(l,r)>\>\")";

default str genImplodeAny(str con, BoolExpr left, BoolExpr right, BOOL allbinds)
	//= NonExhaustive("genImplode", con);
	= "implode<right.con>(T.<Expr2Name(left,right)>)";
///////////////////////////////////////////////////////////////////
private str NonExhaustive(str f, str x)
{
	println("Non-exhaustive pattern in <f> for <x>");
	return "/*<x>*/";
}

private str ToName(str a)
{
	for (t <- ["int", "str", "bool"])
		if (startsWith(a, t))
			return a;
	return "A<a>";
}

private str Expr2Name(BoolExpr l, BoolExpr r)
{
	try
		return ("<r.name>" == "") ? toLowerCase(genSyntax(l)) : "<r.name>";
	catch NoSuchField:
		return toLowerCase(genSyntax(l));
}

private str ReverseDot(str x)
	= intercalate("", reverse(split(".", x)));
