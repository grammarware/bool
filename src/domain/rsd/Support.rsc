@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::rsd::Support

import String;
import ParseTree;
import domain::SyntaxBase;
import domain::rsd::Syntax;

RascalSyntaxDef rsdParse(str x)
	= parse(#RascalSyntaxDef, trim(x));
