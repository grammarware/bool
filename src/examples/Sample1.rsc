@contributor{BOOL}
module Sample1

import ParseTree;

lexical ID = [A-Za-z]+ !>> [A-Za-z];
layout Lay = [\  \t \n];
syntax Foo = ID+;