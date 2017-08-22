@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import IO;
import ParseTree;
import domain::Support;
import Generator;

void main()
{
	for(F <- ["Sample1"])
	{
		T = parseBool(|project://bool/code/<F>.bool|);
		//iprintln(T);
		writeFile(|project://bool/src/examples/<F>.rsc|,
			genHeader(F)
		);
	}
	
}