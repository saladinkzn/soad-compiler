%{
#include <string>
#include "node.h"
#include "parser.hpp"
std::string *buff;
#define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t) (yylval.token = t)
extern "C" int yywrap() { }
%}
%s IN_COMMENT
%s STRING
%%

<INITIAL>[ \t\n]					;
<INITIAL>read						return TOKEN(TREAD);
<INITIAL>if						return TOKEN(IF);
<INITIAL>then						return TOKEN(THEN);
<INITIAL>else						return TOKEN(ELSE);
<INITIAL>while						return TOKEN(WHILE);
<INITIAL>do						return TOKEN(DO);
<INITIAL>elihw						return TOKEN(ELIHW);
<INITIAL>let						return TOKEN(LET);
<INITIAL>be						return TOKEN(BE);
<INITIAL>[a-zA-Z_][a-zA-Z0-9_]* 		SAVE_TOKEN; return TIDENTIFIER;
<INITIAL>[0-9]+\.[0-9]* 			SAVE_TOKEN; return TDOUBLE;
<INITIAL>[0-9]+					SAVE_TOKEN; return TINTEGER;
<INITIAL>"="						return TOKEN(TEQUAL);
<INITIAL>"=="					return TOKEN(TCEQ);
<INITIAL>"!="					return TOKEN(TCNE);
<INITIAL>"<"						return TOKEN(TCLT);
<INITIAL>"<="					return TOKEN(TCLE);
<INITIAL>">"						return TOKEN(TCGT);
<INITIAL>">="					return TOKEN(TCGE);
<INITIAL>"("						return TOKEN(TLPAREN);
<INITIAL>")"						return TOKEN(TRPAREN);
<INITIAL>"{-"         BEGIN(IN_COMMENT);
<IN_COMMENT>"-}"      BEGIN(INITIAL);
<IN_COMMENT>[^-\n]+   // eat comment in chunks
<IN_COMMENT>"-"       // eat the lone star
<IN_COMMENT>\n        yylineno++;
<INITIAL>\"[^"]*\" 			{ yylval.string = new std::string(yytext, 1, yyleng - 2); return TSTRING; }
<INITIAL>"{"						return TOKEN(TLBRACE);
<INITIAL>"}"						return TOKEN(TRBRACE);
<INITIAL>"."						return TOKEN(TDOT);
<INITIAL>","						return TOKEN(TCOMMA);
<INITIAL>"+"						return TOKEN(TPLUS);
<INITIAL>"++"						return TOKEN(TCONCAT);
<INITIAL>"-"						return TOKEN(TMINUS);
<INITIAL>"*"						return TOKEN(TMUL);
<INITIAL>"/"						return TOKEN(TDIV);
<INITIAL>"["						return TOKEN(TSQL);
<INITIAL>"]"						return TOKEN(TSQR);
<INITIAL>";"						return TOKEN(SEPAR);
<INITIAL>"--".*						
<INITIAL>\z						yyterminate();
<INITIAL>.						printf("Unknown token: %s\n", yytext); yyterminate();

%%
