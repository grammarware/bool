@contributor{Vadim Zaytsev - vadim@grammarware.net}
module \syntax::RascalSD

syntax RascalSyntaxDef
	= "star" "[" RascalSyntaxDef inner "]"
	| "plus" "[" RascalSyntaxDef inner "]"
	| "or"   "[" {RascalSyntaxDef ","}+ inners "]"
	| RSDSimpleType
	| RSDAtomic
	;

syntax RSDSimpleType
	= "word"
	;

syntax RSDAtomic
	= "space"
	| "tab"
	| "newline"
	;
