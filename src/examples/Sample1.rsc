@contributor{BOOL}
module Sample1

import ParseTree;

lexical BoolWord = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n];

syntax Foo = BoolWord+;

alias Foo = list[str];
