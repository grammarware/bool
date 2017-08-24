@contributor{BOOL}
module Point

import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;

alias APoint = tuple[int x, int y, value/*method*/ add];
APoint addPoint(Point l, Point r)
{
	return newPoint(l.x+r.x, l.y+r.y);
}
