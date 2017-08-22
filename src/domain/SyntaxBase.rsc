@contributor{Vadim Zaytsev - vadim@grammarware.net}
module domain::SyntaxBase

lexical ID = [A-Za-z]+ !>> [A-Za-z];
layout L = [\ \t \r \n]* !>> [\ \t \r \n];
