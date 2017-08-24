@contributor{BOOL}
module Point

import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;

alias APoint = tuple[int x, int y, APoint(APoint) add];

APoint addPoint(APoint l, APoint r)
	 = newPoint(l.x+r.x, l.y+r.y);

