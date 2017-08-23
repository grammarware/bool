@contributor{Vadim Zaytsev - vadim@grammarware.net}
module ConcreteSyntax

lexical ID = [A-Za-z]+ !>> [A-Za-z];
layout L = [\ \t \r \n]* !>> [\ \t \r \n];

start syntax BOOL = BoolBind*;

syntax BoolBind = ID name ":=" BoolExpr left "~" BoolExpr right;

syntax BoolExpr
	= MultiaryOp con "[" {BoolExpr ","}+ inners "]"
	| UnaryOp con "[" BoolExpr inner "]"
	| NullaryOp con ID? name
	| ID result ":=" RascalExpr expr
	;

syntax MultiaryOp = "or" | "fun" | "seq";
syntax UnaryOp = "list" | "set" | "plus" | "star";
syntax NullaryOp
	= "int" | "str" | "word"
	| "space" | "tab" | "newline" | "comma"
	| "." ;

lexical RascalExpr = ![,\]] >> [,\]] ;
