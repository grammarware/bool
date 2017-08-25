@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Main

import Prelude;
import ParseTree;
import internal::ConcreteSyntax;
import internal::Generator;

void main()
{
	for(F <- ["Simple", "Point", "Rectangle", "Pairs"])
		TransformBoolToRascal(F,
			|project://bool/code/<F>.bool|,
			|project://bool/src/<F>.rsc|);
}
