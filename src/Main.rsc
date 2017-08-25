@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import Prelude;
import ParseTree;
import internal::ConcreteSyntax;
import internal::Generator;

void main()
{
	for(F <- ["Simple", "Point", "Rectangle", "Pairs"])
	{
		T = parse(#start[BOOL],|project://bool/code/<F>.bool|).top;
		// Generate Rascal header
		str text = genHeader(F)
		// Add "standard" library nonterminals
				 + genStandard(T);
		// Complete the concrete syntax part
		for(/BoolBind b := T, !contains("<b.name>", "."))
			text += genSD("<b.name>", b.left, b.right) + "\n";
		// Collect methods' signatures
		map[str,list[str]] methods = ();
		list[str] classes = ["<name>" | /(BoolBind)`<UserId name>:= <BoolExpr _> ~ class[<{BoolExpr ","}+ _>]` := T];
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
		// Add the abstract syntax part
		for(/BoolBind b := T, !contains("<b.name>", "."))
			text += genADT("<b.name>", b.left, b.right, T, methods) + "\n";
		// Concrete to abstract mapping
		for(/BoolBind b := T,
			!contains("<b.name>", "."),
			(BoolExpr)`.` !:= b.right,
			(BoolExpr)`<UserId _>` !:= b.left)
		{
			str c = "<b.name>";
			text += "
					'A<c> implode<c>(C<c> T)
					'	= <genImplosion("<c>", b, T)>;
					'A<c> implode<c>(str input) = implode<c>(parse(#C<c>, input));
					'";
		}
		// Compose clusters of methods
		list[str] processed = [];
		for(str c <- classes, /BoolBind b1 := T, startsWith("<b1.name>", c+"."))
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
		// Serialise into the file
		writeFile(|project://bool/src/<F>.rsc|, text);
	}
	
}