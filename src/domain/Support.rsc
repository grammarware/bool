@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::Support

import IO;
import Prelude;
import ParseTree;
import domain::Syntax;
import domain::SyntaxBase;

import domain::radt::Support;
import domain::rsd::Support;

value(str) getParser(str name, str what)
{
	switch(name)
	{
		case "RascalSD":
			return rsdParse;
		case "RascalADT":
			return radtParse;
		default:
			throw "Unknown <what> language!";
	}
}

Bindings parseBool(loc f)
{
	lines = readFileLines(f);
	Bindings result = ();
	int i = 0;
	while (trim(lines[i])=="") i+=1;
	header = parse(#HeaderBOOL, lines[i]);
	i+=1;
	while (trim(lines[i])=="") i+=1;
	parserSource = getParser("<header.source>", "source");
	parserTarget = getParser("<header.target>", "target");
	
	while(i < size(lines))
	{
		while (trim(lines[i])=="") i+=1;
		list[str] B = split(":=", lines[i]);
		str name = trim(B[0]);
		B = split("~", B[1]);
		if (name in domain(result))
			throw "Name <name> defined twice!";
		result[name] = < parserSource(B[0]), parserTarget(B[1]) >;
		i+=1;
	}		


	return result;
}
