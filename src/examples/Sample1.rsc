@contributor{BOOL}
module Sample1

import ParseTree;

lexical BOOLWord = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n];

syntax Foo = BOOLWord+;

alias Foo = list[str];
