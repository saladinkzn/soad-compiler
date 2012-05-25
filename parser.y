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
%token <string> TIDENTIFIER TINTEGER TDOUBLE TSTRING
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL TCONCAT
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TSQL TSQR
%token <token> SEPAR
%token <token> TREAD
%token <token> IF THEN ELSE
%token <token> WHILE DO ELIHW
%token <token> LET BE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident
%type <expr> numeric string expr cast const_arr
%type <exprvec> arr_body
%type <block> program stmts block
%type <stmt> stmt var_decl
%type <token> comparison unary
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
	  | ite { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  | stmts ite { $1->statements.push_back($<stmt>2); }
	  | stmts cycle { $1->statements.push_back($<stmt>2); }
	  | cycle { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
	  ;
/* TODO: we can move ite to stmts to remove ; after  if */
stmt : expr { $$ = new NExpressionStatement(*$1); } |
	ident expr { $$ = new NExpressionStatement(*(new NMethodCall(*$<ident>1, *$2))); } |
	TREAD ident { $$ = new NExpressionStatement(*(new NReadCall(*$<ident>2))); } |

	var_decl
     ;

var_decl : LET ident BE ident { $$ = new NVariableDeclaration (*$2, *$4, false); } |
	    LET ident BE TSQL ident TSQR { $$ = new NVariableDeclaration (*$2, *$5, true); } |


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

string : TSTRING { $$ = new NString(*$1); }
	
expr : ident TEQUAL expr { $$ = new NAssignment(*$<ident>1, *$3); }
	 | arr 
	 | arr TEQUAL expr { $$ = new NArrAssignment(*$<arr>1, *$3); }
	 | ident { $<ident>$ = $1; }
	 | numeric
	 | string
 	 | expr comparison expr { $$ = new NBinaryOperator(*$1, $2, *$3); }
	 | unary expr { $$ = new NUnaryOperator(*$2, $1); }
     	 | TLPAREN expr TRPAREN { $$ = $2; }
	 | cast
	 | const_arr
	 ;

cast : TLPAREN ident TRPAREN expr { $$ = new NCast(*$<ident>2, *$4, false); } |
	TLPAREN TSQL expr TSQR TRPAREN expr { $$ = new NCast(*$<ident>2, *$6, true); }

arr : ident TSQL expr TSQR { $$ = new NArrayItem(*$<ident>1, *$3); }
	;

const_arr : TLBRACE arr_body TRBRACE { $$ = new NConstArray(*$2); }

arr_body : /*empty*/ { $$ = new std::vector<NExpression*>(); } |
	expr { $$ = new std::vector<NExpression*>(); $$->push_back($1); } |
	arr_body TCOMMA expr { $1->push_back($3); $$ = $1; }


unary : TMINUS ;

comparison : TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE 
		   | TPLUS | TMINUS | TMUL | TDIV | TCONCAT
		   ;

%%
