module \syntax::RascalSD

syntax RascalSyntaxDef
	= "star" "[" RascalSyntaxDef inner "]"
	| "plus" "[" RascalSyntaxDef inner "]"
	| "word"
	;