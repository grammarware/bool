@contributor{Vadim Zaytsev - vadim@grammarware.net}
module Generator

str genHeader(str name)
  = "@contributor{BOOL}
	'module <name>
	'
	'import ParseTree;
	'
	'";
