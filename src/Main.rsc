@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import Prelude;
import ParseTree;
import internal::ConcreteSyntax;
import internal::Generator;

void main()
{
	for(F <- ["Simple", "Point", "Rectangle"])
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
		// Add the abstract syntax part
		for(/BoolBind b := T, !contains("<b.name>", "."))
			text += genADT("<b.name>", b.right, methods) + "\n";
		// Concrete to abstract mapping
		for(/BoolBind b := T, !contains("<b.name>", "."), (BoolExpr)`.` !:= b.right)
		{
			str c = "<b.name>";
			text += "
					'A<c> implode<c>(C<c> T)
					'	= <genImplosion("<c>", b)>;
					'A<c> implode<c>(str input) = implode<c>(parse(#C<c>, input));
					'";
		}
		// Compose clusters of methods
		for(str c <- classes)
			text += "
					'I<c> <c> = \<
					'	<intercalate(",\n", [genMethods(b) | /BoolBind b := T, startsWith("<b.name>", c+".")])>
					'\>;
					'";
		// Serialise into the file
		writeFile(|project://bool/src/<F>.rsc|, text);
	}
	
}