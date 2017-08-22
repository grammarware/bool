@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::radt::Support

import String;
import ParseTree;
import domain::SyntaxBase;
import domain::radt::Syntax;

RascalAlgebraicDataType radtParse(str x)
	= parse(#RascalAlgebraicDataType, trim(x));
