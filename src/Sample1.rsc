@contributor{BOOL}
module Sample1

import IO;
import ParseTree;

lexical BoolWord = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n];

syntax CFoo = BoolWord+;

alias AFoo = list[str];
