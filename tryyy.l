%{
#include <stdio.h>
#include <stdlib.h>
//#include "y.tab.h"
extern int yylineno;
extern char *yytext;


%}
%x CC
%%

[ \t\f\v]  { /*spaces*/}
[\n] {yylineno++;}

"/*"		{ BEGIN(CC);}
<CC>"*/" { BEGIN(INITIAL); }
<CC>. { } 
<CC>\n {yylineno++;}


"if" 	 {return IF;}
"else" 	 {return ELSE;}
"while" 	{ return WHILE;}
"for" 	 {return FOR;}
"var" 	 {return VAR;}
"function" { return FUNCTION;}
"return" 	{ return RETURN;}
"null" 	 {return NUL;}
"void" 	 {return VOID;}
"do" 	 {return DO;}
"&&" 	 {return AND;}
"/" 	 {return DIV;}
"=" 	 {return ASSIGN;}
"==" 	 {return EQ;}
">" 	 {return GREATER;}  
">=" 	 {return GREATER_EQ;}
"<" 	 {return LOWER;}
"<=" 	 {return LOWER_EQ;}
"-" 	 {return MINUS;}
"!" 	 {return NOT;}
"!=" 	 {return N_EQ;}
"||" 	 {return OR;}
"+" 	 {return PLUS;}
"*" 	 {return MULT;}
"&" 	 {return REFERENCE;}
";"	 {return SEMICOLON;}
","	 {return COMMA;}
"|"      {return BAR;}
"{"      {return OPEN_BLOCK;}
"}"      {return CLOSE_BLOCK;}
"("	 {return OPEN_BRACKET;}
")"	{return CLOSE_BRACKET;}
"["  	 {return OPEN_SQUARE;}
"]"	 {return CLOSE_SQUARE;}
true	{return TRUE;}
false	{return FALSE;}
"'"."'" { yylval.string=strdup(yytext) ;return REG_CHAR;}

bool|char|int|real|string {yylval.string=strdup(yytext) ; return TYPE;}
(0|[1-9][0-9]*|0[xX][0-9a-fA-F]+) { yylval.string = strdup(yytext); return REG_INT;}

\"([^\"])*\"  { yylval.string=strdup(yytext); return REG_STRING;}

([a-zA-Z][0-9_]*)+  { yylval.string = strdup(yytext); return IDD;}

([-+]?[0-9]*\.[0-9]*[eE]?[-+]?[0-9]+|[-+]?[0-9]+[eE]?[-+][0-9]+) {yylval.string = strdup(yytext); return REG_REAL;}

.  { yyerror("Error while scanning!"); exit(1); }
%%


