@contributor{BOOL}
module Triangle

import IO;
import ParseTree;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n];

syntax CPoint = BoolInt x "," BoolInt y;
syntax CRectangle = CPoint tl ":" CPoint br;

alias APoint = tuple[int x, int y];
alias IPoint = tuple[APoint(APoint, APoint) add];

APoint newPoint(int x, int y)
	= < x, y >;
alias ARectangle = tuple[APoint tl, APoint br];
alias IRectangle = tuple[ARectangle(APoint, int) square];

ARectangle newRectangle(APoint tl, APoint br)
	= < tl, br >;

IPoint Point = <
	APoint (APoint l, APoint r) { return newPoint(l.x+r.x, l.y+r.y);}
>;

IRectangle Rectangle = <
	ARectangle (APoint tl, int side) { return newRectangle(tl, newPoint(tl.x+side,tl.y+side));}
>;
