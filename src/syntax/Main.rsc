@contributor{Vadim Zaytsev - vadim@grammarware.net}
module \syntax::Main

extend \syntax::RascalSD;
extend \syntax::RascalADT;

syntax BOOL
	= "bind" Language source "~" Language target Binding* bindings L?
	;

syntax Language
	= "RascalSD"
	| "RascalADT"
	;

syntax Binding
	= ID name ":=" RascalSyntaxDef left "~" RascalAlgebraicDataType right
	;

lexical ID = [A-Za-z]+ !>> [A-Za-z];
layout L = [\ \t \r \n]* !>> [\ \t \r \n];
