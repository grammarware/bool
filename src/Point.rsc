@contributor{BOOL}
module Point

import IO;
import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;

alias APoint = tuple[int x, int y];
alias IPoint = tuple[APoint(APoint, APoint) add, APoint(APoint, APoint) sub];

APoint newPoint(int x, int y)
	= < x, y >;

IPoint Point = <
	APoint (APoint l, APoint r) { return newPoint(l.x+r.x, l.y+r.y);},
	APoint (APoint l, APoint r) { return newPoint(l.x-r.x, l.y-r.y);}
>;
