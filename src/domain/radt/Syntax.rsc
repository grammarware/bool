@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::radt::Syntax

import domain::SyntaxBase;

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
