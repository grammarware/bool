@contributor{Vadim Zaytsev - vadim@grammarware.net}
module ConcreteSyntax

lexical UserId = [A-Z][a-z.]* !>> [a-z.];
lexical NormalId = [a-z]+ !>> [a-z];
layout L = [\ \t \r \n]* !>> [\ \t \r \n];

start syntax BOOL = BoolBind*;

syntax BoolBind = UserId name ":=" BoolExpr left "~" BoolExpr right;

syntax BoolExpr
	= MultiaryOp con "[" {BoolExpr ","}+ inners "]"
	| UnaryOp con "[" BoolExpr inner "]"
	| NullaryOp con NormalId? name
	> UserId con NormalId? name
	| UserId con "[" {BoolAssignment ","}+ inners "]"
	;

syntax MultiaryOp = "or" | "fun" | "seq" | "class";
syntax UnaryOp = "list" | "set" | "plus" | "star";
syntax NullaryOp
	= "space" | "tab" | "newline" | "comma"
	| "int" | "str" | "word"
	| "method"
	| "." ;
syntax BoolAssignment = NormalId result ":=" RascalExpr expr;
lexical RascalExpr = ![,\]]+ >> [,\]] ;
