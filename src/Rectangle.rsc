@contributor{BOOL}
module Rectangle

import Prelude;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n]* !>> [\  \t \n];

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

APoint implodePoint(CPoint T)
	= < toInt("<T.x>"), toInt("<T.y>") >;
APoint implodePoint(str input) = implodePoint(parse(#CPoint, input));

ARectangle implodeRectangle(CRectangle T)
	= < implodePoint(T.tl), implodePoint(T.br) >;
ARectangle implodeRectangle(str input) = implodeRectangle(parse(#CRectangle, input));
APoint addPoint (APoint l, APoint r) { return newPoint(l.x+r.x, l.y+r.y);}
ARectangle squareRectangle (APoint tl, int side) { return newRectangle(tl, newPoint(tl.x+side,tl.y+side));}

public IPoint Point = <
	APoint (APoint l, APoint r) { return newPoint(l.x+r.x, l.y+r.y);}
>;

public IRectangle Rectangle = <
	ARectangle (APoint tl, int side) { return newRectangle(tl, newPoint(tl.x+side,tl.y+side));}
>;
