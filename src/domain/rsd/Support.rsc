@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::rsd::Support

import ParseTree;

RascalSyntaxDef rsdParse(str x)
	= parse(#RascalSyntaxDef, x);
