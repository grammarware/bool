@contributor{BOOL}
module examples::Point

import IO;
import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;

alias RPoint = tuple[int x, int y];
alias APoint = tuple[RPoint rep, RPoint(RPoint) add];
APoint newPoint(int x, int y)
{
	APoint res = < <x,y>, RPoint(RPoint a1) { return addPoint(res.rep, a1);} >;
	res.add = RPoint(RPoint a1) { return addPoint(res.rep, a1);};
	//res.x = x;
	//res.y = y;
	//res.add = APoint(APoint a1) { return add(res, a1);};
	return res;
}

RPoint addPoint(RPoint l, RPoint r)
	 = newPoint(l.x+r.x, l.y+r.y).rep;

void t()
{
	q = newPoint(1,2);
	w = newPoint(10,100);
	a = q.add(w.rep);
	println("<a.x>, <a.y>");
}