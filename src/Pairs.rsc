@contributor{BOOL}
module Pairs

import Prelude;

lexical BoolInt = [0-9]+ !>> [0-9];
layout Layout = [\  \t \n]* !>> [\  \t \n];

syntax CPair = BoolInt x "," BoolInt y;
syntax CPoint = CPair;
syntax CComplex = CPoint;

alias APair = tuple[int x, int y];


APair newPair(int x, int y)
	= < x, y >;

alias IPoint = tuple[APair(APair, APair) add, APair(APair, APair) sub];



alias IComplex = tuple[APair(APair, APair) mul, APair(APair, APair) add, APair(APair, APair) sub];



APair implodePair(CPair T)
	= < toInt("<T.x>"), toInt("<T.y>") >;
APair implodePair(str input) = implodePair(parse(#CPair, input));
APair addPoint (APair l, APair r) { return newPair(l.x+r.x, l.y+r.y);}
APair subPoint (APair l, APair r) { return newPair(l.x-r.x, l.y-r.y);}
APair mulComplex (APair l, APair r) { return newPair(l.x*r.x-l.y*r.y, l.y*r.x+l.x*r.y);}
public IPoint Point = <
	APair (APair l, APair r) { return newPair(l.x+r.x, l.y+r.y);},
	APair (APair l, APair r) { return newPair(l.x-r.x, l.y-r.y);}
>;

public IComplex Complex = <
	APair (APair l, APair r) { return newPair(l.x*r.x-l.y*r.y, l.y*r.x+l.x*r.y);},
	APair (APair l, APair r) { return newPair(l.x+r.x, l.y+r.y);},
	APair (APair l, APair r) { return newPair(l.x-r.x, l.y-r.y);}
>;
