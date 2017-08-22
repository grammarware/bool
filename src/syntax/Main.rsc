module \syntax::Main

extend \syntax::RascalSD;
extend \syntax::RascalADT;

syntax BOOL
	= "bind" Language source "~" Language target Binding* bindings
	;

syntax Language
	= "RascalSD"
	| "RascalADT"
	;

syntax Binding
	= ID name ":=" RascalSyntaxDef left "~" RascalAlgebraicDataType right
	;

lexical ID = [a-z]+ !>> [a-z];
layout L = [\ \t \r \n]* !>> [\ \t \r \n];
