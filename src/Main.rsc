@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import \syntax::Main;
import IO;
import ParseTree;

void main()
{
	T = parse(#BOOL, |project://bool/code/Sample1.bool|);
	iprintln(T);
}