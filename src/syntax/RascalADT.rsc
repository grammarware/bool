module \syntax::RascalADT

syntax RascalAlgebraicDataType
	= "list" "[" RascalAlgebraicDataType inner "]"
	| "set" "[" RascalAlgebraicDataType inner "]"
	| "str"
	;
