@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::Support

import IO;
import Prelude;
import ParseTree;
import domain::Syntax;
import domain::SyntaxBase;

Bindings parseBool(loc f)
{
	lines = readFileLines(f);
	Bindings result = ();
	int i = 0;
	while (trim(lines[i])=="") i+=1;
	header = parse(#HeaderBOOL, lines[i]);
	i+=1;
	while (trim(lines[i])=="") i+=1;
	if ((Language)`RascalSD` := header.source
	&&  (Language)`RascalADT` := header.target)
	{
		while(i < size(lines))
		{
			R = parseBindRSDtoRADT(lines[i]);
			if (R.name in domain(result))
				throw "Name <R.name> defined twice!";
			result[R.name] = < R.left, R.right >;
			i+=1;
		}		
	}
	else
		throw "Unsupported pair of languages";
	return result;
}
