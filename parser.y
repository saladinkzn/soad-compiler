%{
	#include "node.h"
        #include <cstdio>
        #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NIfExpr * ite;
	NCycleExpr *cycle;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NVariableDeclaration *var_decl;
	NArrayItem *arr;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NExpression*> *exprvec;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TSQL TSQR
%token <token> SEPAR
%token <token> TREAD
%token <token> IF THEN ELSE
%token <token> WHILE DO ELIHW

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric expr 
%type <block> program stmts block
%type <stmt> stmt
%type <token> comparison
%type <arr> arr
%type <ite> ite
%type <cycle> cycle

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV

%start program

%%

program : stmts { programBlock = $1; }
		;
		
stmts : stmt SEPAR { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts stmt SEPAR { $1->statements.push_back($<stmt>2); }
	  ;
/* TODO: we can move ite to stmts to remove ; after  if */
stmt : expr { $$ = new NExpressionStatement(*$1); } |
	ident expr { $$ = new NExpressionStatement(*(new NMethodCall(*$<ident>1, *$2))); } |
	TREAD ident { $$ = new NExpressionStatement(*(new NReadCall(*$<ident>2))); } |
	ite |
	cycle
     ;

block : TLBRACE stmts TRBRACE { $$ = $2; }
	  | TLBRACE TRBRACE { $$ = new NBlock(); }
	  ;

ite : IF expr THEN block ELSE block { $$ = new NIfExpr(*$2, *$4, *$6); }
cycle : WHILE expr DO block ELIHW { $$ = new NCycleExpr(*$2, *$4); }

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
	  ;

numeric : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
		| TDOUBLE { $$ = new NDouble(atof($1->c_str())); delete $1; }
		;
	
expr : ident TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$3); }
	 | arr 
	 | arr TEQUAL expr { $$ = new NArrAssignment(*$<arr>1, *$3); }
	 | ident { $<ident>$ = $1; }
	 | numeric
 	 | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
     	 | TLPAREN expr TRPAREN { $$ = $2; }
	 ;

arr : ident TSQL expr TSQR { $$ = new NArrayItem(*$<ident>1, *$3); }
	;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE 
		   | TPLUS | TMINUS | TMUL | TDIV
		   ;

%%
