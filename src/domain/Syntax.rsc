@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::Syntax

import domain::SyntaxBase;
import domain::radt::Syntax;
import domain::rsd::Syntax;

syntax HeaderBOOL
	= "bind" Language source "~" Language target
	;

syntax Language
	= "RascalSD"
	| "RascalADT"
	;

syntax BindRSDtoRADT
	= ID name ":=" RascalSyntaxDef left "~" RascalAlgebraicDataType right
	;

alias Bindings = map[str,tuple[RascalSyntaxDef,RascalAlgebraicDataType]];

tuple[str name, RascalSyntaxDef left, RascalAlgebraicDataType right]
	parseBindRSDtoRADT(str s)
{
	R = parse(#BindRSDtoRADT, s);
	return < "<R.name>", R.left, R.right >;
}