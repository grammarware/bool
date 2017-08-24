@contributor{BOOL}
module examples::Point

import IO;
import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;

alias APoint = tuple[int x, int y];

APoint newPoint(int x, int y)
	= <x,y>;

APoint addPoint(APoint l, APoint r)
	 = newPoint(l.x+r.x, l.y+r.y);

alias IPoint = tuple[APoint(APoint,APoint) add];
IPoint Point = < addPoint >;

void t()
{
	q = newPoint(1,2);
	w = newPoint(10,100);
	a = Point.add(q,w);
	println("<a.x>, <a.y>");
}