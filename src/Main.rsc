@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import IO;
import ParseTree;
import domain::Support;

void main()
{
	T = parseBool(|project://bool/code/Sample1.bool|);
	iprintln(T);
}