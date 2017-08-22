@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::radt::Support

import ParseTree;

RascalAlgebraicDataType radtParse(str x)
	= parse(#RascalAlgebraicDataType, x);
