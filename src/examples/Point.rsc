@contributor{BOOL}
module Point

import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax Point = BoolInt x "," BoolInt y;
//syntax Point.add = /*fun[Point one, Point two]*/;

alias Point = tuple[int x, int y, value/*method*/ add];
