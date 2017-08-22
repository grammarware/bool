@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import Prelude;
import ParseTree;
import domain::Support;
import Generator;

void main()
{
	for(F <- ["Sample1"])
	{
		T = parseBool(|project://bool/code/<F>.bool|);
		//iprintln(T);
		str text = genHeader(F) + genStandard(T);
		for(str name <- domain(T))
			text += genSD(name, T[name][0], T[name][1]);
		writeFile(|project://bool/src/examples/<F>.rsc|, text);
	}
	
}