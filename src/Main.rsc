@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import Prelude;
import ParseTree;
import ConcreteSyntax;
import Generator;

void main()
{
	for(F <- ["Sample1", "Point"])
	{
		T = parse(#start[BOOL],|project://bool/code/<F>.bool|).top;
		// Generate Rascal header
		str text = genHeader(F)
		// Add "standard" library nonterminals
				 + genStandard(T);
		// Complete the concrete syntax part
		for(/BoolBind b := T, !contains("<b.name>", "."))
			text += genSD("<b.name>", b.left, b.right) + "\n";
		// Add the abstract syntax part
		for(/BoolBind b := T, !contains("<b.name>", "."))
			text += genADT("<b.name>", b.right) + "\n";
		// Add methods
		for(/BoolBind b := T, contains("<b.name>", "."))
			text += genMethods(b) + "\n";
		// Serialise into the file
		writeFile(|project://bool/src/examples/<F>.rsc|, text);
	}
	
}