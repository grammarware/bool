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

public IPoint Point = <
	APair (APair l, APair r) { return newPair(l.x+r.x, l.y+r.y);},
	APair (APair l, APair r) { return newPair(l.x-r.x, l.y-r.y);}
>;

public IComplex Complex = <
	APair (APair l, APair r) { return newPair(l.x*r.x-l.y*r.y, l.y*r.x+l.x*r.y);},
	APair (APair l, APair r) { return newPair(l.x+r.x, l.y+r.y);},
	APair (APair l, APair r) { return newPair(l.x-r.x, l.y-r.y);}
>;

data DPoint = cpoint(int x, int y, DPoint(DPoint) add, DPoint(DPoint) sub);
data DComplex = ccomplex(int x, int y, APair(APair, APair) mul, APair(APair, APair) add, APair(APair, APair) sub);

public DPoint newPointD(int x, int y) = cpoint(x,y, 
	DPoint(DPoint other) { return newPointD(x + other.x, y + other.y); },
	DPoint(DPoint other) { return newPointD(x - other.x, y - other.y); }
);
//public DComplex newComplexD(int x, int y) = ccomplex(x,y, Complex[0], Complex[1], Complex[2]);

str tostr(DPoint d) = "\<<d.x>,<d.y>\>";

void t()
{
	a = implodePair("1,2");
	b = implodePair("10 , 5");
	println(Point.add(a,b));
	println(Complex.mul(a,b));

	println("-----");
	
	c = newPointD(1,2);
	d = newPointD(10,5);
	println(tostr(c.add(d)));
	//println(Complex.mul(a,b));
}
