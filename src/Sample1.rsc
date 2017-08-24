@contributor{BOOL}
module Sample1

import Prelude;

lexical BoolWord = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n]* !>> [\  \t \n];

syntax CFoo = BoolWord+;

alias AFoo = list[str];
