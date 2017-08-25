@contributor{BOOL}
module Simple

import Prelude;

lexical BoolWord = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n]* !>> [\  \t \n];

syntax CFoo = BoolWord+ boolword;

alias AFoo = list[str];

AFoo implodeFoo(CFoo T)
	= [ "<element>" | BoolWord element <- T.boolword];
AFoo implodeFoo(str input) = implodeFoo(parse(#CFoo, input));
