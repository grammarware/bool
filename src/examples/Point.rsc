@contributor{BOOL}
module Point

import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax Point = BoolInt x "," BoolInt y;
syntax Point.add = /*fun[Point one, Point two]*/;

alias Point = /*class[int x, int y, method add]*/;
alias Point.add = /*Point[x:=one.x+two.x, y:=one.y+two.y]*/;
