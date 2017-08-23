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
		//iprintln(T);
		str text = genHeader(F) + genStandard(T);
		for(/BoolBind b := T)
			text += genSD("<b.name>", b.left, b.right) + "\n";
		for(/BoolBind b := T)
			text += genADT("<b.name>", b.right) + "\n";
		writeFile(|project://bool/src/examples/<F>.rsc|, text);
	}
	
}