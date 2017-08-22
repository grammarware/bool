@contributor{Vadim Zaytsev - vadim@grammarware.net}
module \syntax::RascalADT

syntax RascalAlgebraicDataType
	= "list" "[" RascalAlgebraicDataType inner "]"
	| "set" "[" RascalAlgebraicDataType inner "]"
	| RADTSimpleType
	| RADTAtomic
	;

syntax RADTSimpleType
	= "int"
	| "str"
	;

syntax RADTAtomic
	= "."
	;
