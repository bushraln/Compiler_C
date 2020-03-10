%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


enum KIND{var, function,per};
int countMain =0;
int ind = 1;
int cnt=0;
int label=0;
//const int sizeofword=4; ////in bytes
FILE* output;
int numOfVars=0;
char buffer[100];
int place=0;

typedef struct Node{
char* data;
char* type;
int var;
int nextLabel, trueLabel, falseLabel, begin, after;  //for labeling
char code[500];
char* variable;   
struct Node *node1, *node2, *node3, *node4;
}Node;



typedef struct stcell{
	char* name;
	int index;  // scopes number
	int c;  //counter for number of outer scopes
	struct stcell *next;
	struct stcell *prev;
	struct symbol *sym;
}stcell;


//struct for symbol table line
typedef struct symbol{
	char* name;
	char* type;
	stcell*par;
	enum KIND kind;
	struct symbol *next;
	//struct symbol *innerscope;  
}symbol;


stcell *globalstack;
stcell *globalpar;
//functions declaration
//void checkmain(stcell* );

Node* mknode(char* data, Node* n1, Node* n2, Node* n3, Node* n4);
Node* updateID(Node* n);
void start(Node*);  //start part 2
void visit(Node*);
void fresh_Var(Node*);
void fresh_Var_Id(Node*);
int fresh_Label();
char* intoa(int, char*, int);




extern int yylineno;
extern int yylex();
extern char* yytext;
int yyerror(char * );

%}

%union{
char* string;
struct Node* Node;
}

%token  VOID IF ELSE WHILE DO FOR RETURN NUL VAR FUNCTION STRING
%token  SEMICOLON COMMA OPEN_BRACKET CLOSE_BRACKET OPEN_BLOCK CLOSE_BLOCK OPEN_SQUARE CLOSE_SQUARE BAR
%token  REFERENCE 
%token <string> IDD REG_INT REG_STRING REG_REAL  TYPE TRUE FALSE REG_CHAR NEG
//%token  ASSIGN AND OR N_EQ EQUAL GREATER GREATER_EQ LOWER LOWER_EQ PLUS MINUS MULT DIV NOT

%type <Node>  char func_call ID ret_stmt mult_expr int assignment var_decl in_for m_params params vars bool expr str_index body func
%type <Node> function functions program S void_func stmt nested_block type type_func 
%type <Node>  deref for_loop while_loop else do_while_loop if_stmt lhs pointer some
%type <Node>   in_cond void_fnc_null_params str void_fnc_with_params int_index calc_expr intid 

%right ASSIGN
%left AND OR N_EQ EQ GREATER GREATER_EQ LOWER LOWER_EQ
%left PLUS MINUS
%left MULT DIV
%right NOT
//%precedence NEG /* Negative value (prec tells yacc to use unary minus) */

%%
S:		program { 
		start($1);
		//printTree($1,0);
		
			
		};

program:	functions {$$ = mknode( "", $1, NULL,NULL,NULL);};

functions:	functions function {$$ = mknode("", $1, $2, NULL, NULL);}
		|function  {$$ = $1;};
		
function:	void_func {$$ = $1;}
		|func {$$ = $1;}
		;


void_fnc_with_params:    type_func  VOID ID OPEN_BRACKET m_params CLOSE_BRACKET OPEN_BLOCK body CLOSE_BLOCK  {$$ = mknode("void", $3, $5, mknode("{", $8, NULL, NULL, NULL),mknode("}", NULL, NULL, NULL, NULL));
						$$->node1->nextLabel=fresh_Label(); //added these on sunday
						$$->node3->nextLabel=$$->nextLabel;
						};

// /*in_void:		body  {$$=$1;}
//			;
 //*/
type_func         : FUNCTION {$$ = mknode("FUNCTION",NULL,NULL,NULL,NULL);};

void_fnc_null_params:   type_func VOID ID OPEN_BRACKET CLOSE_BRACKET OPEN_BLOCK body CLOSE_BLOCK
                        {$$ = mknode("void", $3,mknode("{", NULL, NULL, NULL, NULL) , $7, mknode("}", NULL, NULL, NULL, NULL));
						$$->node1->nextLabel=fresh_Label(); //added these on sunday
						
						$$->node3->nextLabel=$$->nextLabel;

						};

void_func:		void_fnc_with_params	{$$ = $1;}
			|void_fnc_null_params	{$$ = $1;};


m_params:	params	 COMMA m_params {$$ = mknode(",", $1, $3, NULL, NULL);}
		|params	{$$ = $1;};


params:		type ID  {$$ = mknode("par", $1, $2, NULL, NULL);};


func:		type_func type ID OPEN_BRACKET m_params CLOSE_BRACKET OPEN_BLOCK body CLOSE_BLOCK
		{$$ = mknode("function", $2, $3, $5, mknode("{", $8, NULL, NULL, mknode("}", NULL, NULL, NULL, NULL)));
		$$->node2->nextLabel=fresh_Label();
        	$$->node4->node1->nextLabel=$$->nextLabel;
			if(strcmp($$->node1->data, "int")==0)     place= place-4;
			if(strcmp($$->node1->data, "char")==0)    place=place-1;
			if(strcmp($$->node1->data, "real")==0)    place=place-4;
			if(strcmp($$->node1->data, "bool")==0)    place=place-1;
			if(strcmp($$->node1->data, "string")==0)  place=place-4;
			if(strcmp($$->node1->data, "char*")==0 || strcmp($$->node1->data, "int*")==0 ||strcmp($$->node1->data, "real*")==0) place=place-4;
		}
		|type_func  type ID OPEN_BRACKET CLOSE_BRACKET OPEN_BLOCK body CLOSE_BLOCK
		{$$ = mknode("function", $2, $3, mknode("{", $7, NULL, NULL,NULL), mknode("}",NULL, NULL, NULL,NULL));
				$$->node2->nextLabel=fresh_Label();
        	         	$$->node3->node1->nextLabel=$$->nextLabel;
				
				if(strcmp($$->node1->data, "int")==0)     place= place-4;
				if(strcmp($$->node1->data, "char")==0)    place=place-1;
				if(strcmp($$->node1->data, "real")==0)    place=place-4;
				if(strcmp($$->node1->data, "bool")==0)    place=place-1;
				if(strcmp($$->node1->data, "string")==0)  place=place-4;
				if(strcmp($$->node1->data, "char*")==0 || strcmp($$->node1->data, "int*")==0 ||strcmp($$->node1->data, "real*")==0) place=place-4;
		
		}
		;




var_decl:       VAR type vars SEMICOLON {$$= mknode("var_decl", $2, $3, NULL, NULL); };

/*str_vars:	STRING str_index SEMICOLON multi_str{$$=mknode("",$2,$4,NULL,NULL);};						
		| {$$=NULL;};*/						

vars:           assignment COMMA vars   {$$ = mknode("", $1, $3, NULL, NULL); 
											}
                |ID COMMA vars  {$$ = mknode(",", $1, $3, NULL, NULL);
								}//i change here
                |assignment     {$$ = mknode("", $1, NULL, NULL, NULL);}
                |ID     {$$ = $1;}
		|str_index	{$$ = mknode("", $1, NULL, NULL, NULL);					}
		|str_index COMMA vars	{$$ = mknode(",", $1, $3, NULL, NULL); }// we need to add this 

;

str_index:	ID OPEN_SQUARE int_index  CLOSE_SQUARE {$$= mknode("str_index", $1,  mknode("[",NULL, NULL, NULL, NULL), $3, mknode ("]", NULL, NULL, NULL, NULL));}
/*multi_str	:str_index ASSIGN str {$$=mknode("",$1,$3,NULL,NULL);}
		|multi_str SEMICOLON {$$=mknode("",$1,NULL,NULL,NULL);}
		;*/


int_index:	int_index PLUS int_index {$$ = mknode("+", $1, $3, NULL, NULL); fresh_Var($$);}
		|int_index MINUS int_index  {$$ = mknode("-", $1, $3, NULL, NULL); fresh_Var($$);}
		|int_index MULT int_index  {$$ = mknode("*", $1, $3, NULL, NULL); fresh_Var($$);}
		|int_index DIV int_index  {$$ = mknode("/", $1, $3, NULL, NULL); fresh_Var($$);}
		|ID  {$$ = mknode("", $1, NULL, NULL, NULL);}// a[5]//i change her	
		|int {$$ = mknode("INTEGER", $1, NULL, NULL, NULL);}
		;


ret_stmt:	RETURN expr SEMICOLON {$$= mknode("return", $2, NULL, NULL, NULL);}
		|RETURN str SEMICOLON	{$$ = mknode("return", $2, NULL, NULL, NULL);}
		|RETURN SEMICOLON	{$$ = mknode("return", NULL, NULL, NULL, NULL);}
		;


body:		function body {$$= mknode("", $1,$2, NULL, NULL);}
		|nested_block body {$$= mknode("body", $1, NULL, NULL, NULL);}
		|stmt body{$$ = mknode("body", $1,$2, NULL, NULL);
			//$$->node1->nextLabel=freshLabel();
		//	$$->node2->nextLabel=$$->nextLabel;

					}
		|	{$$ =NULL;}
;


type:		TYPE  {$$ = mknode($1, NULL, NULL, NULL, NULL);
					if(strcmp($$->data, "int")==0)		place= place+4;
					if(strcmp($$->data, "real")==0)		place= place+4;
					if(strcmp($$->data, "char")==0)		place= place+1;
					if(strcmp($$->data, "bool")==0)		place= place+1;
					if(strcmp($$->data, "string")==0)	place= place+4;
					if(strcmp($$->data, "char*")==0 || strcmp($$->data, "int*")==0 || strcmp($$->data, "real*")==0)		place=place+4;
					};


bool:		TRUE	{$$ = mknode($1, NULL, NULL, NULL, NULL);}
		|FALSE	{$$ = mknode ($1, NULL, NULL, NULL, NULL);};

expr:		ID  {$$ = mknode("", $1, NULL, NULL, NULL);}
		|int  {$$ = mknode("INTEGER", $1, NULL, NULL, NULL);}
		|char	{$$ = mknode("char", $1, NULL, NULL, NULL);}
		|str_index  {$$ = mknode("arr_use", $1, NULL, NULL, NULL);}
		|bool  {$$ = mknode("bool", $1, NULL, NULL, NULL);}
		|BAR ID BAR {$$ = mknode("|", $2,mknode("|",NULL,NULL,NULL,NULL), NULL, NULL);}
		|BAR str BAR	{$$ = mknode ("|" , $2, mknode("|",NULL,NULL,NULL,NULL), NULL, NULL);} 
		|deref {$$ = mknode("", $1 ,NULL, NULL, NULL);}
		|pointer {$$ = mknode("", $1, NULL, NULL, NULL);}
		|MINUS expr %prec NEG	{$$= mknode("-", $2, NULL, NULL, NULL);
								fresh_Var($$);										
								}
		|func_call  {$$ = mknode("", $1, NULL, NULL, NULL);}
		|OPEN_BRACKET expr CLOSE_BRACKET	{
			$$ = mknode("(", $2, NULL, NULL, mknode(")", NULL, NULL, NULL, NULL));
			$$->node1->trueLabel = $$->trueLabel;
			$$->node1->falseLabel = $$->falseLabel;
			$$->variable= (char*)malloc(sizeof($$->node1->variable + 1));
			strcpy($$->variable, $$->node1->variable);
			}
		|NOT expr  {
			$$ = mknode("!", $2, NULL, NULL, NULL);
			$$->node1->trueLabel = $$->falseLabel;
			$$->node1->falseLabel = $$->trueLabel;
			}
		|expr PLUS expr  {$$ = mknode("+", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr MINUS expr  {$$ = mknode("-", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr MULT expr  {$$ = mknode("*", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr DIV expr  {$$ = mknode("/", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr LOWER expr  {$$ = mknode("<", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr GREATER expr  {$$ = mknode(">", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr LOWER_EQ expr {$$ = mknode("<=", $1, $3, NULL, NULL); fresh_Var($$);} 
		|expr GREATER_EQ expr  {$$ = mknode(">=", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr EQ expr  {$$ = mknode("==", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr N_EQ expr {$$ = mknode("!=", $1, $3, NULL, NULL); fresh_Var($$);}
		|expr AND expr {$$ = mknode("&&", $1, $3, NULL, NULL);
						$$->node1->trueLabel = fresh_Label();
						$$->node1->falseLabel = $$->falseLabel;
						$$->node2->trueLabel = $$->trueLabel;
						$$->node2->falseLabel = $$->falseLabel;
																}
		|expr OR expr {$$ = mknode("||", $1, $3, NULL, NULL);
						$$->node1->trueLabel = $$->trueLabel;
						$$->node1->falseLabel = fresh_Label();
						$$->node2->trueLabel = $$->trueLabel;
						$$->node2->falseLabel= $$->falseLabel;								
															}
		;

stmt:		func_call SEMICOLON {$$= mknode("stmt", $1, NULL, NULL, NULL);}
		|assignment SEMICOLON {$$= mknode("stmt", $1, NULL, NULL, NULL);
							//	$$->nextLabel=fresh_Label
										}
	//	|str_vars {$$=mknode("",$1,NULL,NULL,NULL);}
		|while_loop {$$= mknode("stmt", $1, NULL, NULL, NULL);}
		|do_while_loop {$$= mknode("", $1, NULL, NULL, NULL);}
		|var_decl   {$$= mknode("", $1, NULL, NULL, NULL);}
		|for_loop {$$= mknode("", $1, NULL, NULL, NULL);}
		|if_stmt {$$= mknode("stmt", $1, NULL, NULL, NULL);}
		|ret_stmt{$$=$1;} //int foo_4() { if (true) { return 4; } return 10; }
;


func_call:	ID OPEN_BRACKET  mult_expr CLOSE_BRACKET 
		{$$= mknode("func_call", $1, $3,NULL, NULL);}
		|ID OPEN_BRACKET CLOSE_BRACKET
		{$$ = mknode("func_call", $1, NULL, NULL, NULL);}
		;

mult_expr:	expr COMMA mult_expr	{$$ = mknode(",", $1, $3, NULL, NULL);}//i change here 
		|expr  {$$=$1;}
		//|%empty	{$$ = mknode("", NULL, NULL, NULL, NULL);}
		;

assignment:	lhs ASSIGN expr  {$$= mknode( "=", $1, $3, NULL, NULL);}
		|lhs ASSIGN str   {$$ = mknode("=" , $1, $3, NULL, NULL);}
		|lhs ASSIGN NUL  {$$ = mknode("=", $1, mknode( "NULL pointer", NULL, NULL, NULL, NULL),NULL, NULL);};

lhs:		ID	{$$= mknode("", $1, NULL, NULL, NULL);}
		|str_index	{$$ = mknode("arr_use", $1, NULL, NULL, NULL);}
		|deref  {$$ = mknode( "" , $1, NULL, NULL, NULL);};

deref:		REFERENCE some	{$$ = mknode("*", $2, NULL, NULL, NULL);};

some:		OPEN_BRACKET calc_expr CLOSE_BRACKET {$$ = mknode("(", $2, NULL,mknode(")", NULL, NULL, NULL, NULL), NULL);}
		|ID	{$$ = mknode("", $1, NULL, NULL, NULL);}
		;

calc_expr:	calc_expr PLUS intid	{$$ = mknode("+", $1, $3, NULL, NULL);}
		|calc_expr MINUS intid	{$$ = mknode("-", $1, $3, NULL, NULL);}
		|calc_expr DIV intid	{$$ = mknode("/", $1, $3, NULL, NULL);}
		|calc_expr MULT intid	{$$ = mknode("*", $1, $3, NULL, NULL);}
		|int PLUS calc_expr	{$$ = mknode("+", $1, $3, NULL, NULL);}
		|int MINUS calc_expr	{$$ = mknode("-", $1, $3, NULL, NULL);}
		|int DIV calc_expr	{$$ = mknode("/", $1, $3, NULL, NULL);}
		|int MULT calc_expr	{$$ = mknode("*", $1, $3, NULL, NULL);}
		|ID	{$$ = mknode("", $1, NULL, NULL, NULL);}
		;

intid:		ID	{$$ = mknode("", $1, NULL, NULL, NULL);}
		|int	{$$ = mknode("", $1, NULL, NULL, NULL);}
		;

pointer:	REFERENCE ID	{$$ = mknode("&", $2, NULL, NULL, NULL);}
		|REFERENCE str_index  {$$ = mknode("&" ,$2, NULL, NULL, NULL);};  //must check pointer case in visit

str:		REG_STRING  {$$= mknode($1, NULL, NULL, NULL, NULL);};

while_loop:	WHILE OPEN_BRACKET expr CLOSE_BRACKET in_cond
		{$$= mknode( "while", $3, $5, NULL, NULL);
		$$->begin = fresh_Label();
		$$->after = fresh_Label();
		}
		//|WHILE OPEN_BRACKET expr CLOSE_BRACKET stmt
		//{$$ = mknode( "while", $3, $5, NULL, NULL);};
		;

do_while_loop:	DO nested_block WHILE OPEN_BRACKET expr CLOSE_BRACKET SEMICOLON
		{$$ = mknode( "do", $2 , mknode("while", $5, NULL, NULL, NULL), NULL, NULL);};


for_loop:	FOR OPEN_BRACKET in_for SEMICOLON expr SEMICOLON in_for CLOSE_BRACKET in_cond
		{ $$ = mknode( "for", $3, $5, $7, $9);}
		//|FOR OPEN_BRACKET in_for SEMICOLON expr SEMICOLON in_for CLOSE_BRACKET stmt
                //{ $$ = mknode( "for", $3, $5, $7, $9);};
		;

if_stmt:	IF OPEN_BRACKET expr CLOSE_BRACKET in_cond else
                {$$= mknode("IF" , $3, $5,$6 , NULL);
				if($$->node3==NULL){
					$$->node1->trueLabel=fresh_Label();
					$$->node1->falseLabel=$$->nextLabel;
					$$->node2->nextLabel=$$->nextLabel;
				}
				else{
					$$->node1->trueLabel=fresh_Label();
					$$->node1->falseLabel=fresh_Label();
					$$->node2->nextLabel=$$->nextLabel;
					$$->node3->node1->nextLabel=$$->nextLabel;
				}
				}
		;

else:		ELSE in_cond {$$ = mknode("else", $2, NULL, NULL, NULL);}
		|{$$ =NULL;}
		;


in_cond:	nested_block {$$ = mknode("", $1, NULL, NULL, NULL);}
		|stmt  {$$ = mknode("", $1 , NULL, NULL, NULL);}
		;
		
ID:		IDD	{
					$$ = mknode($1,NULL, NULL, NULL, NULL);
					$$= updateID($$);
					fresh_Var_Id($$);
					};

int:		REG_INT	{$$=mknode($1, NULL, NULL, NULL, NULL);
					fresh_Var_Id($$);
					// char* tmp= freshInt();
					// $$->variable= (char*)malloc(sizeof(tmp+1));
					// strcpy($$->variable, tmp );
					};


char:		REG_CHAR {$$ = mknode($1, NULL, NULL, NULL, NULL);
						fresh_Var_Id($$);
						};

nested_block:	OPEN_BLOCK body CLOSE_BLOCK{$$ = mknode("{", $2, NULL, NULL,mknode("}",NULL, NULL, NULL, NULL));}
		;


/*in_block:	var_decls {$$= mknode("", $1, NULL, NULL, NULL);}
		|stmts	{$$ = mknode("", $1, NULL, NULL, NULL);}
		|nested_block {$$ = mknode("", $1, NULL, NULL, NULL);}
		|{$$ =NULL;}
		;
*/



/*stmts:		stmts stmt {$$ = mknode("", $1, $2, NULL, NULL);}
		|stmt {$$= $1;};
*/

in_for:		ID ASSIGN expr {$$ = mknode("=", $1, $3, NULL, NULL);}
		|{$$ =NULL;}
;

%%
#include "lex.yy.c"
main()
{
	return yyparse();
}


int yyerror(char* msg)
{
	fflush(stdout);
	printf("Error %s at line %d\n",msg, yylineno);
	exit(0);
}



Node* mknode(char* data, Node* n1, Node* n2, Node* n3, Node* n4){
	Node* new = (Node*)malloc(sizeof(Node));
	char* newData = (char*) malloc (sizeof(data+1));

	strcpy(newData, data);

	//new->code="";
	new->data = newData;
	new->node1 = n1;
	new->node2 = n2;
	new->node3 = n3;
	new->node4 = n4;
		
	return new;
}


void printTree(Node* n, int tab){
	int i;
	if(n){
		printf("%s\n", n->data);
		for(i=0; i<tab; i++){
			printf(" | ");
		}
		if(n->node1)
			printTree(n->node1, tab+1);
		if(n->node2)
			printTree(n->node2, tab+1);
		if(n->node3)
			printTree(n->node3, tab+1);
		if(n->node4)
			printTree(n->node4, tab+1);
	}
}


void deleteTree(Node* n){
	if(n){
		deleteTree(n->node1);
		deleteTree(n->node2);
		deleteTree(n->node3);
		deleteTree(n->node4);
		free(n);
		}	
}

Node* updateID(Node* n){
	n->var=1;
	//printf("vaaaar\n");
	return n;
}
////////////////////////////////////////////////////////PART TWO - SEMANTICS //////////////////////////////////////////////////////////////////////////////

void make_global_scope(){

   globalstack=(stcell*)malloc(sizeof(stcell));
   globalstack->name=(char*)malloc(sizeof(char));
   strcpy(globalstack->name,"Global");
   globalstack->prev=NULL;
   globalstack->next=NULL;
   globalstack->sym=NULL;
   globalstack->index=0;
   globalstack->c=-1;
}
stcell* push( stcell* head,char*name){
	stcell* tmp = (stcell*) malloc (sizeof(stcell));
	tmp->name=(char*) malloc (sizeof(char));
	if(tmp==NULL)
		exit(0);
	strcpy(tmp->name,name);	
	tmp->index=ind;
	tmp->c= cnt;
	tmp->sym=NULL;

	//tmp->next=head;		
	tmp->next=NULL;

	
	if(cnt==0 ){

		//printf("push in global\n");
		tmp->prev=globalstack;
	}
	else{  if(head!=NULL){
				//printf("push in current\n");
	        	tmp->prev=head;
	}
	}

	head=tmp;
	ind++;
	cnt++;
	//printf("finished pushing\n");
	return head;
}


void free_List(symbol* s){
	symbol *tmp;
	while(s!=NULL){
	tmp=s;
	s=s->next;
	free(tmp);

	
}
}
stcell*  pop( stcell* head){  
	stcell* tmp=head->prev;

	free_List(head->sym);

	ind--;
	free(head);
	return tmp;
}



void printOneSymbol(symbol* s){
	symbol* run2;
    while(s!=NULL){         
        printf("******symbol******\n");

        printf("%s\n",s->name);
        printf("%s\n",s->type);
        printf("%d\n",s->kind);
		if(s->par!=NULL){
 				run2 = s->par->sym;
                printf("parm:");
                while (run2 != NULL)
                {
                    printf(" %s %s\n", run2->type, run2->name);
                    run2 = run2->next;
                }
			}
        s=s->next;
    }
}
void PrintOneStack(stcell *stack){
        

    while(stack!=NULL){
        printf("/////////////stack////////\n");

        printf("%d\n",stack->index);
	 printf("%s\n",stack->name);

            
        printOneSymbol(stack->sym);
        stack=stack->prev;

    }
        
}
void make_global_par(){

   globalpar=(stcell*)malloc(sizeof(stcell));
   globalpar->name=(char*)malloc(sizeof(char));
   strcpy(globalpar->name,"Globalpar");
   globalpar->prev=NULL;
   globalpar->next=NULL;
   globalpar->sym=NULL;
   globalpar->index=0;
   globalpar->c=0;
}
symbol* make_Symol_Table( char* name, char* type, enum KIND kind,stcell *params ){
	//printf("makeSymolTable-%s\n",name);
	symbol* n= (symbol*) malloc (sizeof(symbol));
    n->name = (char*) malloc (sizeof(name+1));
    n->type = (char*) malloc (sizeof(type+1));
    strcpy(n->name, name);
    strcpy(n->type, type);
    n->kind=kind;
	n->par=params;
    n->next=NULL;
	return n;
}

stcell* copy_global_par(stcell*tmp,symbol*sym){
	stcell*tmpnew;
	symbol*tmpsym=sym;
	symbol*tmpsym1;
   tmpnew=(stcell*)malloc(sizeof(stcell));
   tmpnew->name=(char*)malloc(sizeof(char));
   strcpy(tmp->name,tmp->name);
   tmpnew->prev=tmp->prev;
   tmpnew->next=tmp->next;
	tmpnew->index=tmp->index;
   tmpnew->c=tmp->c;
   
   	tmpnew->sym=make_Symol_Table(tmpsym->name,tmpsym->type,tmpsym->kind,NULL);
	tmpsym1=tmpnew->sym;
   while(tmpsym->next!=NULL){
	   tmpsym=tmpsym->next;
	    tmpsym1->next=make_Symol_Table(tmpsym->name,tmpsym->type,tmpsym->kind,NULL);
		tmpsym1=tmpsym1->next;


   }
  // printOneSymbol(tmpnew->sym);
  // tmpnew->sym=sym;
   
   return tmpnew;
}
void  pop_global_pram( ){  
	

	free_List(globalpar->sym);

	free(globalpar);
}
int search_symbol(char *name,char *type, enum KIND kind, stcell *Scope)
	{
	//	printf("search on symbol----%s\n",name);
    symbol *run=NULL;
	// symbol *here=NULL;
	
	if(Scope->sym!=NULL){

    run = Scope->sym;				
	

	
    while (run != NULL)
    {	
	/*
		printf(" symbol name----%s\n",run->name);
				printf(" kind name----%d\n",run->kind);
		printf(" type name----%s\n",run->type);
			printf("  name----%s\n",name);
			printf(" kind ----%d\n",kind);
		printf(" type ----%s\n\n\n\n\n\n\n\n\n\n\n\n",type);*/
        if(kind==0||kind==3){
			//printf("herevar\n");
        	if ((strcmp(name, run->name) == 0 )&&  (strcmp(type, run->type) == 0))
					return 1;}
		else if(kind==1){//printf("herefun\n");
				if ((strcmp(name, run->name) == 0 )&& (kind== run->kind ))
							return 1;}
							
							
        run = run->next;
    }}
    return 0;
}

void look_Up(char *name, char *type,enum KIND kind, stcell *Scope)
{	
	//printf("lookup----%s\n",name);

	symbol*tmp=NULL;
    stcell *run=Scope;
    while(run!=NULL){
		//printf("%d\n",search_symbol(name,type,kind,run));
		if(search_symbol(name,type,kind,run)==1)
			yyerror("variable is already declared in scope!\n");
		//printf("prev---->%s\n",run->prev->name);

		run=run->prev;
	}
	
}
void insertToSym (stcell* head, char* name, char* type, enum KIND kind ,Node*params){
	
	
	//printf("insert %s\n",name);

	stcell *Parmlist;
	symbol *Flist = NULL;
	symbol *N1list = NULL;
	symbol *N2list = NULL;

    int flag=0;
    stcell* tmphead=head;
	symbol *run = NULL;
	symbol *tmp = NULL;
	//sParmlist = NULL;
	if (params != NULL){
	//printf("params->data---->%s\n",params->data);
	if(strcmp(params->data,"par")==0)
		Flist = make_Symol_Table(params->node2->data, params->node1->data,  3, NULL);
    else if (strcmp(params->data,",")==0)
        {
            N2list = Flist;
            do
            {

		//	printf("params->node1->node2->data---->%s\n",params->node1->node2->data);

                N1list = make_Symol_Table(params->node1->node2->data,params->node1->node1->data,  3, NULL); /*בניית סימבולים עבור הפרמטרים*/
               params = params->node2;
	       //printf("here1\n");
		 if(Flist!=NULL){
			   N2list->next = N1list;
			   N2list=N2list->next;	}

                 else{ Flist = N1list;
		       N2list = Flist;} 
				//printf("here2\n");
            	} while (strcmp(params->data,",")==0);
			
			//printf("params->node2->data---->%s\n",params->node2->data);

			N1list = make_Symol_Table(params->node2->data, params->node1->data,  3, NULL); /*בניית סימבולים עבור הפרמטרים*/
			if(N2list!=NULL)
			    N2list->next = N1list;
			    //printf("here3\n");

 	//printOneSymbol(Flist);
        }
        make_global_par(); /*יצירת סקופ המכיל את הפרטנרים*/
        globalpar->sym = Flist;
	Parmlist=copy_global_par(globalpar,globalpar->sym);
	}
	if(cnt==0){
		//printf("insert in global\n");
		if(globalstack->sym==NULL)
			globalstack->sym=make_Symol_Table(  name,  type,  kind,Parmlist);

		else {    run = globalstack->sym;			
			  look_Up(name ,type, kind,globalstack);

			   while (run->next != NULL){//printf("the sym->next not null\n");
        			run = run->next;}
			   tmp=make_Symol_Table(  name,  type,  kind,Parmlist);
			   run->next=tmp;
		}
		if(globalpar!=NULL)
				pop_global_pram();
	}
	else { look_Up(name ,type, kind,tmphead);
		//printf("insert in current\n");
		if(tmphead->sym==NULL)
			tmphead->sym=make_Symol_Table(  name,  type,  kind,NULL);
		else{ 	
			run = tmphead->sym;			
			 while (run->next != NULL){//printf("the sym->next not null\n");
        			run = run->next;}
			tmp=make_Symol_Table(  name,  type,  kind,NULL);
			run->next=tmp;
		}

	head=tmphead;


	}

}

int lookupVar(stcell* stack, char* name){  //check
	//looks up for name starting from top scope to the global scope
	//printf("lookupvar->>>>>%s  \n",name);
	int flag=0;
	symbol* tmp = stack->sym; 
	while(tmp  != NULL && flag==0){
	//printf("%s\n",tmp->name);
		if(strcmp(tmp->name, name)!=0){
			tmp=tmp->next;
		}
		else{
			flag=1;
			
				//printf("flag = %d\n",flag);

			
		
	}}
	if(flag==0 && stack->prev!=NULL){
				
		
			flag=lookupVar(stack->prev, name);
	
	}
	else if(flag==0 && stack->prev==NULL)yyerror("variable isn't declared in scope!lookupvar\n");
	
	return flag;
}
symbol* getSym(stcell* stack, char* name){//i change here
	//printf("getsym->>>>>>\n");
    symbol* sym ;
    stcell* tmp = stack;

    while(tmp!=NULL){
	//printf("%d\n",tmp->index);

        sym=tmp->sym;
        while(sym!=NULL){
			if(strcmp(sym->name, name)!=0){
            		sym=sym->next;}
				else if(strcmp(sym->name, name)==0){									

            				return sym;}

		}
        tmp=tmp->prev;

    
    

	}	
 	return NULL;
}
symbol* getSyminglobal( char* name){//i change here
	//printf("getsym->>>>>>\n");
    symbol* sym ;
    stcell* tmp = globalstack;
    sym=tmp->sym;
    while(sym!=NULL){
		if(strcmp(sym->name, name)!=0){
			sym=sym->next;}

		else if(strcmp(sym->name, name)==0){									

            				return sym->par->sym;
				}

		}
 return NULL;
}
char* checkType(stcell* stack, char* name){
	symbol *s;
	int iffound=lookupVar(stack,name);
	s=getSym (stack, name);
	//printf("%s\n", s->type);
	return s->type;

}

int isInt (stcell* stack, Node* n){

	//printf("isInt entry %s\n", n->data);
	if(strcmp(n->data, "+")==0 || strcmp(n->data, "-")==0 || strcmp(n->data, "*")==0 || strcmp(n->data, "/")==0)
		{	//printf("returning int %d\n", isInt(stack, n->node1)*isInt(stack, n->node2));
			return isInt(stack, n->node1)*isInt(stack, n->node2);}
	else if(strcmp(n->data, "|")==0) //bars needs to be checked
		return 1;
	else if(strcmp(n->data, "INTEGER")==0)
		return 1;
	
	else if(n->var==1){
		//printf("varrr %s\n", checkType(stack, n->data));
		if(strcmp(checkType(stack, n->data), "int")==0)
			return 1;
	}
	else if(strcmp(n->data, "")==0)
		return isInt(stack, n->node1);
	else if(strcmp(n->data, "(")==0){
		//printf("%s node1\n", n->node1->data);
		return isInt(stack, n->node1);
	}
	else{
		//printf("didn't enter\n");
		return 0;
			}
}

void check_str_index(Node* tree,stcell* stack){
	//printf("check_str_index---->\n");
		symbol *here;
			if(tree->node1->data!=NULL&&tree->node1->var==1)
			{	
			   if(lookupVar(stack,tree->node1->data)==1){
				here=getSym(stack,tree->node1->data);
				if(strcmp(here->type,"int")!=0)
				  yyerror("Index of string must be integer!");
			      }
			}	

			
			else if(isInt(stack,tree)!=1)
					yyerror("Index of string must be integer!");

}

int isBoolean (stcell* stack, Node* n){
	//	printf("%s entered isboolean\n",n->data);
	if(strcmp(n->data, "bool")==0)
		return 1;

	else if(n->var==1)
	{ 
	//printf("before checktype %s\n", checkType(stack, n->data));
		if(strcmp(checkType(stack, n->data),"bool")==0){
			return 1;
		}
		else
			return 0;	
	}
	else if(strcmp(n->data,"")==0&&n->node1->var==1)
	{ 
	//printf("before checktype\n");
	//printf("before checktype %s\n", checkType(stack, n->node1->data));
		if(strcmp(checkType(stack, n->node1->data),"bool")==0){
			return 1;
		}
		else
			return 0;	
	}
	else if(strcmp(n->data, "+") ==0 || strcmp(n->data, "-") ==0 || strcmp(n->data, "*") ==0 || strcmp(n->data, "/") ==0  ){
		return 0;
	}

	else if(strcmp(n->data, "<")==0 || strcmp(n->data, ">")==0 || strcmp(n->data, ">=")==0 || strcmp(n->data, "<=")==0){
		//printf("isInt node1? %d\n", isInt(stack, n->node1));
		//printf("isInt node2? %d\n", isInt(stack, n->node2));
		if(isInt(stack, n->node1)==1 && isInt(stack, n->node2)==1)
			return 1;
		else
			return 0;
	}
	
	else if(strcmp(n->data, "==" )==0 || strcmp(n->data, "!=")==0){
		if(isBoolean(stack, n->node1)==1 && isBoolean(stack, n->node2) ==1)
			return 1;
		if(isInt(stack, n->node1)==1 && isInt(stack, n->node2)==1)
			return 1;
		else
			return 0;
	}


	else if(strcmp(n->data, "||")==0 || strcmp(n->data, "&&")==0 || strcmp(n->data, "!")==0 ){
		//printf(" node1? %s\n",  n->node1->data);
		//printf(" node2? %s\n",n->node2->data);
		if(n->node1 && n->node2){
			return isBoolean(stack, n->node1) * isBoolean(stack, n->node2);}
		else if(n->node1)
			return isBoolean(stack, n->node1);
	}
	else if(strcmp(n->data, "(")==0)
		return isBoolean(stack, n->node1);
	else{
		//printf("entered else\n");
		return 0;
	}
	//printf("out\n");
}


Node* search(Node* tree, const char* data)
{  
	Node* result=NULL;
    if (tree==NULL)
		return NULL;
	if(0==strcmp(tree->data,data))
		return tree;
	result=search(tree->node1,data);
	if (result!=NULL)
		return result;
	result=search(tree->node2,data);
	if (result!=NULL)
		return result;
	result=search(tree->node3,data);
	if (result!=NULL)
		return result;
	result=search(tree->node4,data);
	return result;

	
}
void checkReturn(Node* tree,stcell* stack){//Q9


	Node *tmp=tree;
	Node *tmp1;
	symbol*sym;
	//printf("checkReturn -->%s \n",tree->data);

	
	//tmp =search(tree,"void");
	if(strcmp(tmp->data,"void")==0){	
			
	//printf("%s\n",tmp->data);

	tmp1=search(tmp,"return");
		
	if(tmp1!=NULL)
		if(tmp1->node1 !=NULL)
				yyerror("void can't return !!\n");
		
	//printf("good\n");
	}
		
	else {
	tmp=tree;
	tmp1=NULL;
	
	//char *typefun;
	
	//tmp =search(tree,"func");

	//printf("%s\n",tmp->data);

	if(strcmp(tmp->data,"function")==0){
	//	printf("checkReturnfun \n");
					if(strcmp(tmp->node1->data,"string")==0)
							yyerror("The Function Can't Return String");			


						
					tmp1 =search(tmp,"return");
					//printf("tmp1->data %s \n",tmp1->data);

					//printf("tmp1->node1->data %s \n",tmp1->node1->data);//+
					//printf("tmp1->node1->node1->node1->data %s \n",tmp1->node1->node1->node1->data);
					//printf("%d rgr\n",tmp1->node1->node1->var);

					if(tmp1!=NULL){						
												
								//printf("tmp1->node1->node1->data %s \n",tmp1->node1->node1->data);
								//printf("tmp1->node1->data %s \n",tmp1->node1->data);//+
								if(tmp1->node1==NULL)
									yyerror("This function most return something!\n");
								else { 
								 if(strcmp(tmp1->node1->data,"INTEGER")==0){
									if(strcmp(tmp->node1->data,"int")!=0)
										yyerror("the return value of the functios are illegal!1!\n");
								}
								else if(strcmp(tmp1->node1->data,"bool")==01){
									if(strcmp(tmp->node1->data,"bool")!=0)
										yyerror("the return value of the functios are illegal!2!\n");
								}
								else if(strcmp(tmp1->node1->data,"char")==0){
									if(strcmp(tmp->node1->data,"char")!=0)
										yyerror("the return value of the functios are illegal!3!\n");
								}
								else
									if(isBoolean(stack,tmp1->node1)==1){
										
									//	printf("isBoolean-%d\n",isBoolean(stack,tmp1->node1));
									//	printf("%s\n",tmp1->node1->node1->data);
											if(strcmp(tmp->node1->data,"boolean")!=0)
													yyerror("the return value of the functios are illegal!4!\n");
									//	printf("some1\n");
									}
								else if(isInt(stack,tmp1->node1)==1){
												if(strcmp(tmp->node1->data,"int")!=0)
													yyerror("the return value of the functios are illegal!5!\n");
									//	printf("some2\n");
									}
								else if(tmp1->node1->node1!=NULL&&(tmp1->node1->node1->var==1||strcmp(tmp1->node1->node1->data,"str_index")==0||strcmp(tmp1->node1->node1->data,"func_call")==0)){
											
														//	printf("someee1\n");
														//	printf("type func--> %s\n",tmp->node1->data);
														//	printf("data func--> %s\n",tmp1->node1->node1->data);

													//printf("data func--> %s\n",tmp1->node1->node1->node1->data);


										if(tmp1->node1->node1->var==1){

										if(lookupVar(stack,tmp1->node1->node1->data)){
										sym=getSym(stack,tmp1->node1->node1->data);
										if(strcmp(sym->type,tmp->node1->data)!=0)
												yyerror("the return value of the functios are illegal!6!\n");

										}}
										else if(strcmp(tmp1->node1->node1->data,"str_index")==0) {

												if(strcmp(tmp->node1->data,"char")!=0)
													yyerror("the return value of the functios are illegal!!7\n");
								}
										else if(strcmp(tmp1->node1->node1->data,"func_call")==0) {
											if(lookupVar(stack,tmp1->node1->node1->node1->data)){

												sym=getSym(stack,tmp1->node1->node1->node1->data);
														//printf("type func--> %s\n",sym->type);
												if(strcmp(tmp->node1->data,sym->type)!=0)
													yyerror("the return value of the functios are illegal!!8\n");
										}	}
								}	
								else {yyerror("the return type not good\n");}
						
							}		
					}else yyerror("This function most return something!\n");

				
	}
							
	}

}
void checkPointer(Node* tree,stcell* stack){//q18


	symbol* here;
	int iffound=0;
	
	
	Node *tmp=tree;


	//tmp =search(tree,"&");
	if(tmp!=NULL){
		
	if(strcmp(tmp->node1->data,"str_index")==0){
		if(lookupVar(stack,tree->node1->node1->data))
				check_str_index(tree->node1->node3,stack);

	}
	else{

	 iffound=lookupVar(stack,tmp->node1->data);
	 	if(iffound==1){
		here=getSym(stack,tmp->node1->data);
		if(strcmp(here->type,"int")!=0 &&strcmp(here->type,"char")!=0 && strcmp(here->type,"real")!=0)
		yyerror("& can't work with  anything besides int or char!");

		}
	
	}
	}
}

char* isPointer(symbol* s){
	if(strcmp(s->type, "int*")==0)
		return "int*";
	else if (strcmp(s->type, "char*")==0)
		return "char*";
	else if (strcmp(s->type, "real*")==0)
                return "real*";
	else
		return NULL;
}
char* checkExpr(stcell* stack, Node* tree){
	//printf("in checkExpr %s\n", tree->data);
    if(strcmp(tree->data, "(")==0){
        return checkExpr(stack, tree->node1);
    }

	else if(tree->var==1){
		//printf("entered gaol\n");
		return getSym(stack, tree->data)->type;
	}

    else if(strcmp(tree->data, "")==0){  //another expr
        if(strcmp(tree->node1->data, "str_index")==0){
            return "char";
        }

        else if(strcmp(tree->node1->data, "*")==0){
		//	printf("ok\n");
            if(tree->node1->node1->node1->var==1){
                return getSym(stack, tree->node1->node1->node1->data)->type;
        }
            else
                return checkExpr(stack, tree->node1->node1);
        }


        else if(strcmp(tree->node1->data, "&")==0){
            if(strcmp(tree->node1->node1->data, "str_index")==0){
                return "char*";
            }
            else{
                return getSym(stack, tree->node1->node1->data)->type; ///if it's var

            }
        }

        else if(strcmp(tree->node1->data, "func_call")==0){
            if(lookupVar(stack, tree->node1->node1->data)){
                return getSym(stack, tree->node1->node1->data)->type;
                }
        }

        else{  //if expr(rhs) is id
            //printf("almost\n");
            return getSym(stack, tree->node1->data)->type;
            
        }
    }

    else if(isInt(stack, tree)==1){   //if rhs is integer 
     //  printf("%d blabla\n",isInt(stack, tree));
	    return "int";
	
    }

	else if(isBoolean(stack, tree)==1){
		return "bool";
	}

	else if(strcmp(tree->data, "+")==0 || strcmp(tree->data, "-")==0 || strcmp(tree->data, "*")==0 || strcmp(tree->data, "/")==0){
		if(tree->node1->node1->var==1)
			return checkExpr(stack, tree->node1);
		if(tree->node2->node1->var==1)
			return checkExpr(stack, tree->node2);
	}
    /*else{ ///if rhs is string literal
        printf("it's string!\n");
        return "string";
    }*/
}
//checks that * is applied\used only on pointers and returns type of pointer int*\char*\real*\NULL 
char* checkDeref(stcell* stack, Node* n){ ////have to get the ^ node
	//printf("check deref %s\n", n->data);
	char *ans1, *ans2;
	if(strcmp(n->node1->data, "")==0){
		if(lookupVar(stack,n->node1->node1->data )==1 && getSym(stack, n->node1->node1->data)!=NULL){
			symbol* s = getSym(stack, n->node1->node1->data);
			return isPointer(s);
		}
	}
	else{
		//printf("in der %s\n", n->node1->data);
		if(strcmp(checkExpr(stack, n->node1), "int*")== 0){
		//	printf("in deref\n");
			return "int*";
		}
	
	}
}
int countSEonTree( Node*tree,int count){
   

    if(strcmp(tree->data, ",")==0){
			count++;

	//printf("countSEonTree %d\n",count);
        if(tree->node2){
            //printf("node1\n");
             count=countSEonTree(tree->node2,count);
        }
        
	} 
	else{
	//printf("%d\n",count);
	return count;
	}
}

int  countParinFunc(symbol*sym){
	symbol*symtmp=sym;
		int count=0;
	//printf("countParinFunc\n");
		while(symtmp!=NULL){
				printf("rrrr\n");
	printf(" symtmp %s\n",symtmp->name);
	printf(" symtmp %d\n",symtmp->kind);

			
				count++;
			symtmp=symtmp->next;
		}
	return count;
}
void checkargument(Node* tree,stcell* stack,symbol*sym){
	Node *tmp=tree;

	char *s;
	//printf("checkargument in-%s\n",tmp->node1->data);
	//printf("checkargument in-%d\n",tmp->node1->var);
	//	s=checkType(stack,tmp->node1->data);

	if(strcmp(tmp->data,"char")==0){//printf("e----%s\n",tmp->node1->data);//else if expr
			if(strcmp(sym->type,"char")!=0)
				yyerror("the arguments not match!!\n");}
	else if(strcmp(tmp->data,"real")==0){
			if(strcmp(sym->type,"real")!=0)
				yyerror("the argument not match!!\n");}
	else if(strcmp(tmp->data,"INTEGER")==0){

			if(strcmp(sym->type,"int")!=0)
				yyerror("the arguments not match!!\n");}
	else  if(strcmp(tmp->data,"bool")==0){

			if(strcmp(sym->type,"bool")!=0)
				yyerror("the arguments not match!!\n");}
	else if(isInt(stack,tmp->node1)==1){//printf("var2\n");
			if(strcmp(sym->type,"int")!=0)
				yyerror("the arguments not match!!\n");}
	else if(isBoolean(stack,tmp->node1)==1){

			if(strcmp(sym->type,"bool")!=0)
				yyerror("the arguments not match!!\n");}
	else 	if( tmp->node1->var==1){
		//printf("var77\n");
		s=checkType(stack,tmp->node1->data);
	
		if(strcmp(s,sym->type)!=0){
			yyerror("the arguments not match!!\n");
		}
	}
		printf("done checkargument \n");


}
void CheckArgumentOnFun(Node* tree,stcell* stack,symbol*sym,int count){//Q8
	//printf("here\n");
	Node *tmp=tree;
	symbol *symtmp=sym;
	symbol *here=sym;
	char *s;
	int flag=0;

	int counttmp=count;
	//printf("in--->%s\n",tmp->data);
	//printf("in--->%s\n",tmp->node1->data);


	/*	while(symtmp!=NULL&&flag==0){
			if(symtmp->kind==3){
				flag=1;
				here=symtmp;
				}
			symtmp=symtmp->next;

		}*/
	if(tmp->node2->data!=NULL){
 	if(count>1&&strcmp(tmp->node2->data,",")==0){
		//  printf("e1-%s\n",tmp->node2->node1->node1->data);
		//printf("e1-%s\n",tmp->node2->node1->node1->data);

		checkargument(tmp->node2->node1,stack,here);
			counttmp--;
			tmp=tmp->node2;
		//printf("sym-%s\n",here->name);

			here=here->next;
	

		while(counttmp>1&&strcmp(tmp->node2->data,",")==0){
		 /// printf("e2-%s\n",tmp->node2->node1->node1->data);
		//	printf("sym-%s\n",here->name);

			checkargument(tmp->node2->node1,stack,here);


			counttmp--;
			tmp=tmp->node2;

			here=here->next;

 		}
		//printf("e3-%s\n",tmp->node2->node1->data);
		//printf("sym-%s\n",here->name);

		checkargument(tmp->node2,stack,here);

	 
	 }
	 else if(count==1){
		

		checkargument(tmp->node2,stack,here);
			}

}}

void checkFuncCall(Node* tree,stcell* stack){//q7
	symbol* here;
	int iffound=0;
	int counter=0;
	int countSE=0;
	Node *tmp=tree;
	Node *tmp1;

	//tmp =search(tree,"func_call");
	if(tmp!=NULL){

		iffound=lookupVar(stack,tmp->node1->data);

	 	if(iffound==1){
			here=getSyminglobal(tmp->node1->data);
			//printf(" here %s\n",here->name);
				counter=countParinFunc(here);
			if(tmp->node2!=NULL){
				countSE=countSEonTree(tmp->node2,countSE);
			//	printf("countSE=%d\n",countSE);
			//	printf("counter=%d\n",counter);

				if(strcmp(tmp->node2->data,",")==0)
					{
						if(countSE+1 !=counter){
							yyerror("This Function can't accept those arguments!!! ");
						}
						
					}else if(counter!=1){//printf("count=%d\n",counter);
								yyerror("This Function can't accept  arguments!!! ");
								}

			}else if(counter!=0)
				yyerror("This Function with   arguments!!! ");
	
	//printf("done checkFuncCall \n");
	if(counter>=1){
	CheckArgumentOnFun(tmp,stack,here,counter);
	}
	}
	}
}
int checkAssign (stcell* stack, Node* tree){ //have to get the '=' node

    char *lhsType, *rhsType, *tmp;
    symbol *lhs, *rhs;
   //printf("is %s\n", tree->node1->node1->data);


    if(tree->node1 && tree->node1->var==1){  //suitable for loop body
        //printf("before getsym\n");
        lhs = getSym(stack, tree->node1->data);
        if(lhs!=NULL){
            //printf("after getsym\n");
            lhsType = (char*) malloc(sizeof(lhs->type+1));
            strcpy(lhsType, lhs->type);
            //printf("");
        }
        else
            yyerror("lhs is not defined!");
    }
    else if(tree->node1->node1->var == 1){ //if lhs id ID
        //printf("before getsym\n");
        lhs = getSym(stack, tree->node1->node1->data);
        if(lhs!=NULL){
            //printf("after getsym\n");
            lhsType = (char*) malloc(sizeof(lhs->type+1));
            strcpy(lhsType, lhs->type);
            //printf("");
        }
        else
            yyerror("lhs is not defined!");
    }
    else if(strcmp(tree->node1->node1->data, "str_index")==0){  //if lhs is str_index
        lhs = getSym( stack, tree->node1->node1->node1->data);
        if(lhs!=NULL){
            lhsType = (char*)malloc(sizeof("char"+1));
            strcpy(lhsType, "char");
        }
        else
            yyerror("lhs is not defined!");
    }
    else{  //if lhs is dereference
		//printf("checking deref %s\n", tree->node1->node1->data);
        if(checkDeref (stack, tree->node1->node1) != NULL ){
			//printf("after checking deref\n");
            lhsType = (char*) malloc(sizeof(checkDeref (stack, tree->node1->node1->node1)+1));
            strcpy(lhsType, checkDeref (stack, tree->node1->node1->node1));
        }
        else
            yyerror("lhs isn't pointer! ");
        
    }



    ///////checking rhs
   // printf("before %s\n", tree->node2->data);
    if(strcmp(tree->node2->data, "NULL pointer")==0){  //if rhs is null
        //printf("after\n");
        if(strcmp(lhsType, "int*")==0 || strcmp(lhsType, "char*")==0)
            return 1;
        else
            yyerror("variable cannot be assigned to null!\n");
    }


    else if(isBoolean(stack, tree->node2)==1){   //if rhs is boolean
        if(strcmp(lhsType, "bool")==0 )
            return 1;
        else
            yyerror("hs type must be boolean, unmatched types in assignment!\n");
    }

    else if(strcmp(tree->node2->data, "char")==0){  //if rhs is char
        if(strcmp(lhsType, "char")==0)
            return 1;
        else
            yyerror("lhs type must be char, unmatched types in assignment!\n");
    }

    ////check if rhs is another expr

    else if(strcmp(tree->node2->data, "|")==0){
        if(tree->node2->node1->var==1){
            rhs = getSym(stack, tree->node2->node1->data);
            if(rhs){
                rhsType = (char*)malloc(sizeof(rhs->type+1));
                //printf("rhs type %s\n", rhsType);
                strcpy(rhsType, rhs->type);
                if(strcmp(rhsType, "int")==0 || strcmp(rhsType, "string")==0){
                    if(strcmp(lhsType, "int")==0 )
                        return 1;
                    else
                        yyerror("BARS return integer value, unmatched types in assignment!\n");
                }
                else
                    yyerror("BARS should be used only on integers or strings!\n");
            }
            else{
                if(strcmp(lhsType, "int")==0 )
                        return 1;
                    else
                        yyerror("BARS return integer value, unmatched types in assignment!\n");
            }
        }
        
    }

    else if(strcmp(tree->node2->data, "(")==0){

            tmp= checkExpr(stack, tree->node2);
            if(tmp){
            rhsType = (char*)malloc(sizeof(rhs->type+1));
            strcpy(rhsType, tmp);
            if(strcmp(lhsType, rhsType)==0)
                return 1;
            else
                yyerror("in parenthesis: unmatched assignment types!\n");
                
            }
    }

    
    else if(strcmp(tree->node2->data, "")==0){


        if(strcmp(tree->node2->node1->data, "str_index")==0){
            if(strcmp(lhsType, "char")==0)
                return 1;
            else
                yyerror("lhs type have to be char , unmatched types in assignment!\n");
        }

        else if(strcmp(tree->node2->node1->data, "*")==0){
            if(tree->node2->node1->node1->node1->var==1){
            rhs = getSym(stack, tree->node2->node1->node1->node1->data);
            if(rhs!=NULL){
                rhsType = (char*)malloc(sizeof(rhs->type+1));
                strcpy(rhsType, rhs->type);
	   if(strcmp(rhsType, "int*")==0 && strcmp(lhsType, "int")==0)
                    return 1;
                else if (strcmp(rhsType, "char*")==0 && strcmp(lhsType, "char")==0)
                    return 1;
		else if (strcmp(rhsType, "real*")==0 && strcmp(lhsType,"real")==0)
		    return 1;
                else
                    yyerror("unmatched dereference type!\n");
            }
        }

        else{
            tmp= checkExpr(stack, tree->node2->node1->node1);
            if(tmp){
            rhsType = (char*)malloc(sizeof(rhs->type+1));
            strcpy(rhsType, tmp);
            if(strcmp(rhsType, "int*")==0 && strcmp(lhsType, "int")==0)
                    return 1;
                else if (strcmp(rhsType, "char*")==0 && strcmp(lhsType, "char")==0)
                    return 1;
		else if (strcmp(rhsType, "real*")==0 && strcmp(lhsType, "real")==0)
			return 1;
            else
                yyerror("in parenthesis: unmatched assignment types!\n");
                }
        }
        }
            
        else if(strcmp(tree->node2->node1->data, "&")==0){
            if(strcmp(tree->node2->node1->node1->data, "str_index")==0){
                if(strcmp(lhsType, "char*")==0)
                    return 1;
                else
                    yyerror("lhs must be char pointer!\n");
            }
            else 
                rhs= getSym(stack, tree->node2->node1->node1->data);
                if(rhs!=NULL){
                rhsType = (char*)malloc(sizeof(rhs->type+1));
                strcpy(rhsType, rhs->type);
                if(strcmp(lhsType, "int*")==0 && strcmp(rhsType, "int")==0)
                    return 1;
                else if (strcmp(lhsType, "char*")==0 && strcmp(rhsType, "char")==0)
                    return 1;
		else if( strcmp(lhsType, "real*")==0 && strcmp(rhsType, "real")==0)
			return 1;
                else
                    yyerror("unmatched pointer type!\n");
            }
            }

        else if(strcmp(tree->node2->node1->data, "func_call")==0){
            if(lookupVar(stack, tree->node2->node1->node1->data)){
            rhs = getSym(stack, tree->node2->node1->node1->data);
            if(rhs!=NULL){
                rhsType = (char*)malloc(sizeof(rhs->type+1));
                strcpy(rhsType, rhs->type);
                if(strcmp(lhsType, rhsType)==0)
                    return 1;
            }
            yyerror("lhs type doesn't match function return type!\n");
        }
        }
        else{   //if rhs is id
           // printf("llllllll %s\n", tree->node2->node1->data);
            rhs = getSym(stack, tree->node2->node1->data);
            //printf("id\n");
            if(rhs!=NULL){
                rhsType = (char*)malloc(sizeof(rhs->type+1));
                strcpy(rhsType, rhs->type);
				if(strcmp(tree->node1->node1->data, "*")==0){
					 if(strcmp(lhsType, "int*")==0 && strcmp(rhsType, "int")==0)
                    	return 1;
					 if(strcmp(lhsType, "char*")==0 && strcmp(rhsType, "char")==0)
                    	return 1;
					if(strcmp(lhsType, "real*")==0 && strcmp(rhsType, "real")==0)
			return 1;
				}
                if(strcmp(lhsType, rhsType)==0)
                    return 1;
            }
            yyerror("id type in rhs doesn't match type of lhs!\n ");
        }
    
    }
    

    else if(isInt(stack, tree->node2)==1){   //if rhs is integer 
    //    printf("after isInt\n");
        if(strcmp(lhsType, "int")==0 )
            return 1;
        else
            yyerror("lhs type must be int, unmatched types in assignment!\n");
    }

    else if(strcmp(tree->node2->data, "+")==0 || strcmp(tree->node2->data, "-")==0 || strcmp(tree->node2->data, "*")==0 || strcmp(tree->node2->data, "/")==0){
        
		tmp= checkExpr(stack, tree->node2);
		//printf("tmp %s\n", tmp);
        if(tmp){
            rhsType = (char*)malloc(sizeof(rhs->type+1));
            strcpy(rhsType, tmp);
            if(strcmp(lhsType, rhsType)==0)
                return 1;
            else
                yyerror("complicated expr: unmatched assignment types!\n");
    }
    }

    else if(strcmp(tree->node2->data, "<")==0 || strcmp(tree->node2->data, ">")==0 || strcmp(tree->node2->data, ">=")==0 || strcmp(tree->node2->data, "<=")==0 || strcmp(tree->node2->data, "==" )==0 || strcmp(tree->node2->data, "!=")==0 || strcmp(tree->node2->data, "!")==0){
        tmp= checkExpr(stack, tree->node2);
		//printf("tmp in boolean %s\n", tmp);
        if(tmp){
            rhsType = (char*)malloc(sizeof(rhs->type+1));
            strcpy(rhsType, tmp);
            if(strcmp(lhsType, rhsType)==0)
                return 1;
            else
                yyerror("complicated expr: unmatched assignment types!\n");
    }
    }

    else{//printf("herer!\n"); ///if rhs is string literal
        if(strcmp(lhsType, "string")==0)
            return 1;
        else
            yyerror("lhs must be string!\n");
    }
    
    return 0;
}





stcell* tree_to_st(stcell* stack, Node* tree){
	//printf("tree_to_st--%s\n",tree->data);
	
	if(strcmp(tree->data, "void")==0){
		if(strcmp(tree->node1->data,"main")==0){
			countMain++;
			if(countMain>1){
				yyerror("There are more than one Main function!");
			}
			if(strcmp(tree->node2->data,"{")!=0)
 				yyerror("Main function can't get any args!");	
		}
	
		
		if(strcmp(tree->node3->data,"{")==0)//with par
			{
				insertToSym (stack, tree->node1->data ,tree->data, 1 ,tree->node2);
					
		stack=push(stack,tree->node1->data);

			if(tree->node1)
				stack = tree_to_st(stack, tree->node1);
			if(tree->node2)
				stack = tree_to_st(stack, tree->node2);
			if(tree->node3)
				stack = tree_to_st(stack, tree->node3->node1);
			if(tree->node4)
			{checkReturn(tree,stack);
					stack = tree_to_st(stack, tree->node4);
			}
			}
			else if(strcmp(tree->node2->data,"{")==0)
			{
				insertToSym (stack, tree->node1->data ,tree->data, 1 ,NULL);
		stack=push(stack,tree->node1->data); 
				if(tree->node1)
					stack = tree_to_st(stack, tree->node1);
	//			if(tree->node2)
	//				stack = tree_to_st(stack, tree->node2);
				if(tree->node3)
					stack = tree_to_st(stack, tree->node3);
				if(tree->node4){
					checkReturn(tree,stack);
					stack = tree_to_st(stack, tree->node4);
				}


			}
		

	}//end of if void
	else if(strcmp(tree->data, "function")==0){
			if(strcmp(tree->node1->data,"main")==0){
			countMain++;
			if(countMain>1){
				yyerror("There are more than one Main function!");
			}
			if(strcmp(tree->node2->data,"{")!=0)
 				yyerror("Main function can't get any args!");	
		}
		if(strcmp(tree->node1->data,"string")==0)
				yyerror("The Function Can't Return String");


			 
		if(strcmp(tree->node4->data,"{")==0)//with par
			{
				insertToSym (stack, tree->node2->data ,tree->node1->data, 1 ,tree->node3);
		
			stack=push(stack, tree->node2->data);
				
			if(tree->node1)
				stack = tree_to_st(stack, tree->node1);
			if(tree->node2)
				stack = tree_to_st(stack, tree->node2);
			if(tree->node3)
				stack = tree_to_st(stack, tree->node3);
			if(tree->node4){
					stack = tree_to_st(stack, tree->node4->node1);
					checkReturn(tree,stack);
					stack = tree_to_st(stack, tree->node4->node4);
							}		

			}
			
		else if(strcmp(tree->node3->data,"{")==0)
			{
				insertToSym (stack, tree->node2->data ,tree->node1->data, 1 ,NULL);

			stack=push(stack, tree->node2->data);
			if(tree->node1)
					stack = tree_to_st(stack, tree->node1);
			if(tree->node2)
					stack = tree_to_st(stack, tree->node2);
			if(tree->node3)
					stack = tree_to_st(stack, tree->node3->node1);
			if(tree->node4){
				checkReturn(tree,stack);
					stack = tree_to_st(stack, tree->node4);
		
			}

			}

	}//end if if func
	else if(strcmp(tree->data, "{")==0){
				stack=push(stack,"Block");
			 	if(tree->node1)
					stack = tree_to_st(stack, tree->node1);
				if(tree->node2)
					stack = tree_to_st(stack, tree->node2);
				if(tree->node3)
					stack = tree_to_st(stack, tree->node3);
				if(tree->node4)
					stack = tree_to_st(stack, tree->node4);

	}
	else if(strcmp(tree->data, "}")==0){
				cnt--;
				//PrintOneStack(stack);
				//printf("$$$$$$$$$$$$$$$$$$$444\n");
				stack=pop(stack);
				//PrintOneStack(stack);
				}
	
	
	else if(strcmp(tree->data, "var_decl")==0){
			
		 if(tree->node2->node2 !=NULL){

			 			//	printf("some\n");
			//printf("node  %s\n",tree->node2->data);
			if(strcmp(tree->node2->data, ",")==0){

				
				if(tree->node2->node1->node1 !=NULL){
		 		if(strcmp(tree->node2->node1->data, "str_index")==0){
					 if(strcmp(tree->node1->data,"string")!=0)
					 	yyerror("array can't be anything besides string!");
					check_str_index(tree->node2->node1->node3,stack);
					insertToSym(stack,tree->node2->node1->node1->data, tree->node1->data, 0,NULL);
				}	 
				}else{ insertToSym(stack, tree->node2->node1->data, tree->node1->data, 0,NULL);}
			





				if(tree->node2->node2!=NULL&&strcmp(tree->node2->node2->data, ",")==0){
					//flag=1;
					if(tree->node2->node2->node1->node1 !=NULL){
		 		if(strcmp(tree->node2->node2->node1->data, "str_index")==0){
					insertToSym(stack,tree->node2->node2->node1->node1->data, tree->node1->data, 0,NULL);
				}	 
				}else{ insertToSym(stack, tree->node2->node2->node1->data, tree->node1->data, 0,NULL);}
			
				if(tree->node2->node2->node2->node1 !=NULL){
		 		if(strcmp(tree->node2->node2->node2->node1->data, "str_index")==0){
					insertToSym(stack,tree->node2->node2->node2->node1->node1->data, tree->node1->data, 0,NULL);
				}	 
				}else{ insertToSym(stack, tree->node2->node2->node2->data, tree->node1->data, 0,NULL);}
			



				}

				if(tree->node2->node2->node1 !=NULL){

		 		if(strcmp(tree->node2->node2->node1->data, "str_index")==0){
					insertToSym(stack,tree->node2->node2->node1->node1->data, tree->node1->data, 0,NULL);
				}	 
				}
				else {insertToSym(stack, tree->node2->node2->data, tree->node1->data, 0,NULL);}
			

					
			}



			}	
			else if(tree->node2->node1 !=NULL){
						//printf("some1\n");

		 		if(strcmp(tree->node2->node1->data, "str_index")==0){
					 if(strcmp(tree->node1->data,"string")!=0)
					 	yyerror("array can't be anything besides string!");
					check_str_index(tree->node2->node1->node3,stack);
					insertToSym(stack,tree->node2->node1->node1->data, tree->node1->data, 0,NULL);
					 
				}
			}	
										 		//		printf("some2\n");

			else	insertToSym(stack, tree->node2->data, tree->node1->data, 0,NULL);
			
		
	}	
	else if(strcmp(tree->data,"&")==0)
						checkPointer(tree,stack);
	else if(strcmp(tree->data,"func_call")==0)
						checkFuncCall(tree,stack);
	else if(strcmp(tree->data, "*")==0){
			if(checkDeref(stack, tree)!=NULL)
				printf("dereference is ok!\n");
	}
	else if(strcmp(tree->data, "IF")==0){
			//printf("checking %s\n", tree->data);
			if(isBoolean(stack, tree->node1)!=1)
				//printf("if condition is ok!\n");
				yyerror("expression in condition must be boolean!\n");
		}
	else if(strcmp(tree->data, "for")==0){
			if(isBoolean(stack, tree->node2)!=1)
				//printf("for condition is ok!\n");
				yyerror("expression in for loop must be boolean\n");
	}
	else if(strcmp(tree->data, "while")==0){
			if(isBoolean(stack, tree->node1)!=1)
				//printf("while condition is ok!\n");
				yyerror("expression in while loop must be boolean!\n");
	}
	else if(strcmp(tree->data, "+")==0 || strcmp(tree->data, "|")==0 || strcmp(tree->data, "-")==0 || strcmp(tree->data, "*")==0 || strcmp(tree->data, "/")==0 || strcmp(tree->data, "INTEGER")==0){
			if(isInt(stack, tree)==1)
				printf("isInt is ok!\n");
	}
	else if(strcmp(tree->data, ">")==0 || strcmp(tree->data, "<")==0 || strcmp(tree->data, ">=")==0 || strcmp(tree->data, "<=")==0 || strcmp(tree->data, "==")==0 || strcmp(tree->data, "!=")==0 || strcmp(tree->data, "!")==0 || strcmp(tree->data, "&&")==0 || strcmp(tree->data, "||")==0){
			if(isBoolean(stack, tree)==1)
				printf("isBoolean is ok!\n");}
	else if(strcmp(tree->data, "=")==0){
			//printf("in assign\n");
			if(checkAssign(stack, tree)!=1)
				//printf("assign ok!\n");
				yyerror("problem in assignment!\n");
		}
	else if(strcmp(tree->data, "par")==0){
			insertToSym(stack, tree->node2->data, tree->node1->data, 3,NULL);
		
	}
	else{
		if(tree->node1)
			stack = tree_to_st(stack, tree->node1);
		if(tree->node2)
			stack = tree_to_st(stack, tree->node2);
		if(tree->node3)
			stack = tree_to_st(stack, tree->node3);
		if(tree->node4)
			stack = tree_to_st(stack, tree->node4);


	}	
	return stack;
}



void start(Node* tree){
	if(tree){
		stcell*stack;
		make_global_scope();
		//printTree(tree,0);
		stack=tree_to_st(stack,tree);
		if(countMain==0){
			yyerror("you don't have a function main !");
		}
	//	visit(tree);

	
		//printf("dooooone\n");
		

	}

}











//////////////////////////////////////////PART3 - CODE GENERATION!!!!!!!!!!!!!!!!!!!!!!!/////////////////////////////


void storeToFile(char code[]){
	output = fopen("output.txt", "a");
	if(output){
		
		fputs(code, output);
		fclose(output);
	}
}


int fresh_Label(){
	return ++label;
}

void fresh_Var_Id(Node* tree){
	tree->variable = (char*)malloc(sizeof(tree->data + 1));
	strcpy(tree->variable, tree->data);
}

void fresh_Var(Node* tree){
	numOfVars++;
	place=place+4;
	char* numch= intoa( numOfVars, buffer, 10);
	char* t= (char*)malloc(sizeof("t"+1));
	strcpy(t, "t");
	strcat(t, numch);
	tree->variable = (char*)malloc(sizeof(t+1));
	strcpy(tree->variable, t);
}
char* fresh_Int(){
	numOfVars++;
	place+=4;
	char* numch = intoa(numOfVars, buffer, 10);
	char* t= (char*)malloc(sizeof("t"+1));
	strcpy(t, "t");
	strcat(t, numch);
	return t;
}

void ifnoelse(Node* tree){
		//printf("eeee\n");

	strcpy(tree->code, tree->node1->code);
	strcat(tree->code, tree->node2->node1->code);
	strcat(tree->code, "\nL");
	strcat(tree->code, intoa(tree->nextLabel, buffer, 10));
	strcat(tree->code, ":\n");
	strcat(tree->code, "\n");
	//printf("%s\n", tree->code);
}


void ifwithelse(Node* tree){
	
	strcpy(tree->code, tree->node1->code);
	strcat(tree->code, "L");
	strcat(tree->code, intoa(tree->trueLabel, buffer, 10));
	strcat(tree->code, ": ");
	strcat(tree->code, tree->node2->code);
	strcat(tree->code, " goto L");
	strcat(tree->code, intoa(tree->nextLabel, buffer, 10));
	strcat(tree->code, "\nL");
	strcat(tree->code, intoa(tree->node1->falseLabel, buffer, 10));
	strcat(tree->code, " : ");
	strcat(tree->code, tree->node3->node1->code);
	
	//printf("%s\n", tree->code);
	strcpy(tree->code, tree->node1->code);
	strcat(tree->code, tree->node2->node1->code);
	strcat(tree->code, "\nL");
	strcat(tree->code, intoa(tree->node1->falseLabel, buffer, 10));
	strcat(tree->code, ":\n");
	strcat(tree->code, tree->node3->node1->node1->code);
}



void while_stmt(Node* tree){
	/*
	char* tmp = intoa(tree->begin, buffer, 10);
	strcpy(tree->code, tmp);
	strcat(tree->code, ": ");
	strcat(tree->code, tree->node1->code);
	strcat(tree->code, "\nif");
	strcat(tree->code, tree->node1->variable);
	strcat(tree->code, " = 0 goto L");
	strcat(tree->code, intoa(tree->after, buffer, 10));
	strcat(tree->code, tree->node2->code);
	strcat(tree->code, "\ngoto L");
	strcat(tree->code, tmp);
	strcat(tree->code, intoa(tree->after, buffer, 10));
	strcat(tree->code, " : ");
	printf("%s\n", tree->code);
	*/
	char* tmp = intoa(tree->begin, buffer, 10);
	strcat(tree->code, "\nL");
	strcat(tree->code, tmp);
	strcat(tree->code, " : ");
	strcat(tree->code, "\n");

	strcat(tree->code, tree->node1->code);
	strcat(tree->code, tree->node2->node1->code);

	strcat(tree->code, "\ngoto L");
	strcat(tree->code, intoa(tree->begin, buffer, 10));
	strcat(tree->code, ";");
	
	strcat(tree->code, "\nL");
	strcat(tree->code, intoa(tree->node1->falseLabel, buffer, 10));
	strcat(tree->code, ":");
	strcat(tree->code, "\n");
}


void dowhile(Node* tree){
	
}


void andOp (Node* tree){
	strcpy(tree->code, tree->node1->code);
	//printf("in and %d\n", tree->node2->falseLabel);
	//strcat(tree->code, "ifz")
	strcat(tree->code, "L");
	strcat(tree->code, intoa(tree->node1->trueLabel, buffer, 10 ));
//	printf("%s\n", tree->code);
	strcat(tree->code, ":");
	//printf("%s\n", tree->node1->node1->variable);
	strcat(tree->code, tree->node2->code);
//	printf("%s\n", tree->code);
}

void orOp (Node* tree){
	strcpy(tree->code, tree->node1->code);
//	printf("%s\n", tree->code);
	strcat(tree->code, "L");
	strcat(tree->code, intoa(tree->node1->falseLabel, buffer, 10 ));
	//printf("%s\n", tree->code);
	strcat(tree->code, ":");
	//printf("%s\n", tree->node1->node1->variable);
	strcat(tree->code, tree->node2->code);
	//printf("%s\n", tree->code);
}

void notOp(Node* tree){

	strcpy(tree->code, tree->node1->code);
	//printf("%s\n", tree->code);

}


void funcall(Node* tree){
	if(tree->node2){ //if it has parameters
				if(strcmp(tree->node2->data, ",")==0){
				//	printf("comma %s\n", tree->node2->node2->data);
					strcat(tree->code, "\nPushParam ");
					strcat(tree->code, tree->node2->node2->node1->variable);
					strcat(tree->code, ";\n");
					strcat(tree->code, "PushParam ");
					//if(tree->node2->node2->node1->node1->variable)
					strcat(tree->code, tree->node2->node1->node1->variable);
					strcat(tree->code, ";");
					// if(tree->node2->node2)
					// 	visit(tree->node2->node2);

					//problem!! doesn't accept more than two arguments
				}
				else{
					//printf("in else %s\n", tree->node2->node1->variable);
					strcat(tree->code, "\nPushParam ");
					strcat(tree->code, tree->node2->node1->variable);
					strcat(tree->code, ";");
				}
			}
	//printf("%s\n", tree->code);
}
char* str_indx(Node* tree){
		char *tmp1= fresh_Int();
		// char *prev = (char*)malloc(sizeof("t"+1));
		// strcpy(prev, "t");
		// strcat(prev, intoa(numOfVars-1, buffer, 10));
		// printf("orev is %s\n", prev);
		char *tmp2= fresh_Int();
		char *tmp3= fresh_Int();
		char *tmp4= fresh_Int();

		//printf("begin test\n");
		
		
		if(tree->node1->node3->node1->variable){
			//printf("test 1 %s\n", tree->node1->node3->node1->data);
			strcat(tree->code, tmp1);
			strcat(tree->code, " = ");
			strcat(tree->code,tree->node1->node3->node1->variable );
			
		}
		else{
		//	printf("test 2 %s\n", tree->node1->node3->data);
			strcat(tree->code, tree->node1->node3->code);
			strcat(tree->code, tmp1);
			strcat(tree->code, " = ");
			strcat(tree->code, tree->node1->node3->variable );
		}
		
		strcat(tree->code, ";\n");
		//printf("in between %s\n", tree->code);
		strcat(tree->code, tmp2);
		strcat(tree->code, " = ");
		strcat(tree->code, "4;");
		strcat(tree->code, ";\n");
		strcat(tree->code, tmp3);
		strcat(tree->code, " = ");
		strcat(tree->code,tmp1 );
		strcat(tree->code, " * ");
		strcat(tree->code,tmp2);
		strcat(tree->code, ";\n");
		strcat(tree->code, tmp4);
		strcat(tree->code, " = ");
		if(tree->node1->node1->variable)
			strcat(tree->code, tree->node1->node1->variable);
		else
			strcat(tree->code, tree->node1->node1->code);
		//printf("test end %s\n", tree->node1->node1->data);
		strcat(tree->code, " + ");
		strcat(tree->code, tmp3);
		strcat(tree->code, ";\n");
		//printf("str indx %s\n", tree->code);
		return tmp4;
}

void parenthesis(Node* tree){
	tree->node1->trueLabel = tree->trueLabel;
	tree->node1->falseLabel = tree->falseLabel;
	strcpy(tree->code, tree->node1->code);
}

void trueLit(Node* tree){

}

void falseLit(Node* tree){

}

/*
void idstmt(Node* tree){
	strcpy(tree->variable, tree->data);
	strcpy(tree->code, "");
}
*/

void visit(Node* tree){
	//printf("visit-->%s\n",tree->data);
	if(tree){

		if(strcmp(tree->data, "body")==0){
			if(tree->node2){//printf("here body\n");
				tree->node2->nextLabel=tree->nextLabel;}
		}

		if(tree->node1)
			visit(tree->node1);
		if(tree->node2)
			visit(tree->node2);
		if(tree->node3)
			visit(tree->node3);
		if(tree->node4)
			visit(tree->node4);


	if(strcmp(tree->data, "{")==0){
		//printf("here {\n");
		if(tree->node1)
				strcpy(tree->code, tree->node1->code);

			}
	//if(strcmp(tree->data, "in_cond")==0){
			//strcpy(tree->code, tree->node1->code);
	//}
	if(strcmp(tree->data, "stmt")==0){  //need to be checked on all stmt gzirot
				//printf("stmt\n");
			//	if(strcmp(tree->data, "func_call")==0)
				
				strcpy(tree->code, tree->node1->code);

				if(strcmp(tree->node1->data, "func_call")==0){
					funcall(tree->node1);
					strcat(tree->code, tree->node1->code);
					strcat(tree->code, "\nLcall ");
					strcat(tree->code, tree->node1->node1->data);
					strcat(tree->code, ";\n");

					if(tree->node1->node2){
					strcat(tree->code, "PopParams ");
					strcat(tree->code, ";\n");
					//printf(" %s\n", tree->code);
					}
				}
				//strcat(tree->code, "\n");
				//if(tree->node2)
				//	strcat(tree->code,tree->node2->code);
				//strcat(tree->code, intoa(tree->node1->nextLabel, buffer, 10));
				//strcat(tree->code, "\n");
				//printf("%s\n", tree->code);

				
			}

		if(strcmp(tree->data, "return")==0){
			
			if(tree->node1){
				//printf("in return %s\n", tree->node1->code);
				strcpy(tree->code, tree->node1->code);
				strcat(tree->code, "\nreturn ");
				if(tree->node1->node1->variable)
					strcat(tree->code, tree->node1->node1->variable);
				else
					strcat(tree->code, tree->node1->variable);
			}
			else
				strcat(tree->code, "\nreturn;");
			//printf("%s\n", tree->code);
		}

		if(strcmp(tree->data, "body")==0){
			//printf("in body %s\n", tree->node1->code);
			if(strcmp(tree->node1->data, "return")==0)
				strcat(tree->code, tree->node1->code);
			else
				strcpy(tree->code, tree->node1->code);
			if(tree->node2)
				strcat(tree->code, tree->node2->code);
		}	



		if(strcmp(tree->data, "void")==0){
		
			//need to add "begin func" & "end func"

			strcpy(tree->code, tree->node1->data);
			strcat(tree->code, ":\n");
			strcat(tree->code, "BeginFunc ");
			strcat(tree->code, intoa(place, buffer, 10));
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node3->code);
			strcat(tree->code, "\nEndFunc;\n\n\n");
			strcat(tree->code , "-------------------------------\n");
			//printf("%s\n", tree->code);
			storeToFile(tree->code);
		}

		if(strcmp(tree->data, "function")==0 ){
			if(strcmp(tree->node3->data,"{")==0){
			strcpy(tree->code, tree->node2->data);
            strcat(tree->code, ":\n");
            strcat(tree->code, "BeginFunc ");
            strcat(tree->code, intoa(place, buffer, 10));
            strcat(tree->code, "\n");
            strcat(tree->code, tree->node3->node1->code);
            strcat(tree->code, "\nEndFunc;\n\n\n");
			strcat(tree->code , "-------------------------------\n");
			}
			else {
				strcpy(tree->code, tree->node2->data);
            strcat(tree->code, ":\n");
            strcat(tree->code, "BeginFunc ");
            strcat(tree->code, intoa(place, buffer, 10));
            strcat(tree->code, "\n");
            strcat(tree->code, tree->node4->node1->code);
            strcat(tree->code, "\nEndFunc;\n\n\n");
			strcat(tree->code , "-------------------------------\n");
			}
            //printf("%s\n", tree->code);
			storeToFile(tree->code);
		}


		// if(strcmp(tree->data, ",")==0){
		// 	//printf("in comma %s\n", tree->node2->node1->variable);
		// 	if(tree->node2->variable==NULL && tree->node2->node1->variable!=NULL){
		// 		tree->node2->variable= (char*)malloc(sizeof(tree->node2->node1->node1->variable+1));
		// 		strcpy(tree->node2->variable, tree->node2->node1->variable);
		// 	}
		// }

		

		if(strcmp(tree->data, "=")==0){

		//printf("id data is %s\n", tree->node2->code);

		//printf("id type is %s\n", tree->node1->node1->type);
		strcpy(tree->code, tree->node2->code);

		if(strcmp(tree->node2->data, "arr_use")==0 && strcmp(tree->node1->data, "arr_use")==0){
			//printf("no\n");
		}
		
		else if(strcmp(tree->node2->data, "arr_use")==0 && strcmp(tree->node1->data, "arr_use")!=0 ){
			//printf("ffff %s\n", tree->node2->code);
			char* tmp = str_indx(tree->node2);
			//char* help = fresh_Int();
			strcat(tree->code, "\n");
			if(tree->node2->node1->node3->node1->variable)
				strcat(tree->code, tree->node2->node1->node3->code);
			strcat(tree->code, tree->node2->code);
			strcat(tree->code, "\n");
			//strcat(tree->code, help);
			strcat(tree->code, tree->node1->node1->variable);
			strcat(tree->code, " = ");
			strcat(tree->code, "*(");
			strcat(tree->code, tmp);
			strcat(tree->code, ");\n");
			//printf("%s\n", tree->code);
		}

		else if(strcmp(tree->node2->data, "arr_use")!=0 && strcmp(tree->node1->data, "arr_use")==0){
			//printf("gggggg %s\n", tree->node1->node1->node3->node1->variable);
			char* tmp1= str_indx(tree->node1);
			strcat(tree->code, "\n");
			//printf("arr use %s\n", tree->node1->node1->code);
			if(tree->node1->node1->node3->node1->variable)
				strcat(tree->code, tree->node1->node1->node3->code);
			strcat(tree->code, tree->node1->code);
			strcat(tree->code, "\n*(");
			strcat(tree->code, tmp1);
			strcat(tree->code, ")");
			strcat(tree->code, " = ");
			if(tree->node2->node1)
				if(tree->node2->node1->variable)
					strcat(tree->code, tree->node2->node1->variable);
				else
					strcat(tree->code, tree->node2->variable);
			else
				strcat(tree->code, "NULL");
			strcat(tree->code, ";\n");
		}

		else if(tree->node2->node1 && strcmp(tree->node2->node1->data, "func_call")==0){
				funcall(tree->node2->node1);
				strcat(tree->code, tree->node2->node1->code);
				strcat(tree->code, "\n");
				strcat(tree->code, tree->node1->node1->variable);
				strcat(tree->code, " = ");
				strcat(tree->code, "Lcall ");
				strcat(tree->code, tree->node2->node1->node1->data);
				strcat(tree->code, ";\n");
				if(tree->node2->node1->node2){
					strcat(tree->code, "PopParams ");
					strcat(tree->code, ";\n");
				//printf(" %s\n", tree->code);
				}
		}
		else{
			//printf("in assign %s\n", tree->node2->node1->code);
			strcat(tree->code, tree->node1->node1->variable);
			strcat(tree->code, " = ");
			if(tree->node2->node1){
				if(tree->node2->node1->variable)
					strcat(tree->code, tree->node2->node1->variable);
				else
					strcat(tree->code, tree->node2->variable);
			}
			else
				strcat(tree->code, "NULL");
			strcat(tree->code, ";\n");
		}

		/*
		if(strcmp(tree->node1->data, "arr_use")==0){
			char* tmp1= str_indx(tree->node1);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node1->code);
			strcat(tree->code, "\n*(");
			strcat(tree->code, tmp1);
			strcat(tree->code, ")");

		}
		else{
			strcat(tree->code, tree->node1->node1->variable);
		}
		//printf("%s\n", tree->node1->code);
		strcat(tree->code, "=");
		if(strcmp(tree->node2->data, "arr_use")==0){
			
			char* tmp = str_indx(tree->node2);
			char* help = fresh_Int();
			//printf("ffff %s\n", tree->node2->code);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
			strcat(tree->code, "\n");
			strcat(tree->code, help);
			strcat(tree->code, " = ");
			strcat(tree->code, "*(");
			strcat(tree->code, tmp);
			strcat(tree->code, ");\n");
			//printf("%s\n", tree->code);
		}
		//strcat(tree->code, " = ");
		else{
		if(tree->node2->node1)
			if(tree->node2->node1->variable)
				strcat(tree->code, tree->node2->node1->variable);
			else
				strcat(tree->code, tree->node2->variable);
		else
			strcat(tree->code, "NULL");
		strcat(tree->code, ";\n");
		
		}*/
		}

		if(strcmp(tree->data, "&&")==0){
			//printf("somethind\n");
			andOp(tree);

		}
		if(strcmp(tree->data, "||")==0){
			orOp(tree);
		
		}
	
		if(strcmp(tree->data, "while")==0){
			while_stmt(tree);
		}

		if(strcmp(tree->data, "do")==0){
			dowhile(tree);
		}

		if(strcmp(tree->data, "IF")==0 && tree->node3 == NULL){
			//printf("if no else \n");
			ifnoelse(tree);
		}


		if(strcmp(tree->data, "IF")== 0 && tree->node3!=NULL){
			ifwithelse(tree);
		}


		if(strcmp(tree->data, "(")==0){
			strcpy(tree->code, tree->node1->code);
			
		}
		if(strcmp(tree->data, "+")==0){
			char* tmp;
			//printf("five %s\n", tree->node2->code);
			//printf("somr %s\n", tree->node2->node1->data);
			if(tree->node2->node1 && strcmp(tree->node2->node1->data, "func_call")==0){
					funcall(tree->node2->node1);
					strcat(tree->code, tree->node2->node1->code);
					strcat(tree->code, "\n");
					strcat(tree->code, tree->variable);
					strcat(tree->code, " = ");
					strcat(tree->code, "Lcall ");
					strcat(tree->code, tree->node2->node1->node1->data);
					strcat(tree->code, ";\n");
					//strcat(tree->code, tree->node1->code);
					//strcat(tree->code, tree->node2->node1->code);
					//strcat(tree->code, "\n");
					//printf("one %s\n", tree->variable);
					tmp=fresh_Int();
					strcat(tree->code,tmp);
					//strcat(tree->code, tree->node2->node1->variable);
					//printf("two %s\n", tree->code);
					strcat(tree->code, "=");
					//printf("three %s\n", tree->node1->node1->variable);
					if(tree->node1->node1->variable)
						strcat(tree->code, tree->node1->node1->variable);
					else
						strcat(tree->code, tree->node1->variable);
					//printf("four %s\n", tree->variable);
					strcat(tree->code, "+");
					strcat(tree->code, tree->variable);
					strcat(tree->code, "\n");
					if(tree->node2->node1->node2){
					strcat(tree->code, "PopParams ");
					strcat(tree->code, ";\n");
					strcpy(tree->variable, tmp);
					//printf(" %s\n", tree->code);
					}
			}
			else{
				strcpy(tree->code, tree->node1->code);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
			//printf("one %s\n", tree->code);
			strcat(tree->code, tree->variable);
			//printf("two %s\n", tree->code);
			strcat(tree->code, "=");
			//printf("three %s\n", tree->node1->node1->variable);
			if(tree->node1->node1->variable)
				strcat(tree->code, tree->node1->node1->variable);
			else
				strcat(tree->code, tree->node1->variable);
			//printf("four %s\n", tree->code);
			strcat(tree->code, "+");
				if(tree->node2->node1->variable!=NULL)
					strcat(tree->code, tree->node2->node1->variable);
				else
					strcat(tree->code, tree->node2->variable);
			}
			strcat(tree->code, "\n");
			//printf("plus code %s\n", tree->code);
		}

		if(strcmp(tree->data, "-")==0){
			if(tree->node2){
			strcpy(tree->code, tree->node1->code);
			//strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
		//	printf("%s\n", tree->code);
			strcat(tree->code, tree->variable);
		//	printf("%s\n", tree->code);
			strcat(tree->code, "=");
			//printf("%s\n", tree->node1->node1->variable);
			strcat(tree->code, tree->node1->node1->variable);
		//	printf("%s\n", tree->code);
			strcat(tree->code, "-");
			//printf("%s\n", tree->code);
			if(tree->node2->node1->variable)
				strcat(tree->code, tree->node2->node1->variable);
			else
				strcat(tree->code, tree->node2->variable);
			strcat(tree->code, ";\n");
			}
			else{
				strcpy(tree->code, tree->node1->code);
				strcat(tree->code, "\n");
				strcat(tree->code, tree->variable);
			//	printf("%s\n", tree->code);
				strcat(tree->code, "=");
				//printf("%s\n", tree->node1->node1->variable);
				strcat(tree->code, "-");
				//printf("%s\n", tree->code);
				if(tree->node1->node1->variable)
					strcat(tree->code, tree->node1->node1->variable);
				else
					strcat(tree->code, tree->node1->variable);
				strcat(tree->code, ";\n");
			}
			//printf("%s\n", tree->code);
		}
		
		if(strcmp(tree->data, "*")==0){
			
			strcpy(tree->code, tree->node1->code);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
			//printf("one %s\n", tree->code);
			strcat(tree->code, tree->variable);
			//printf("two %s\n", tree->code);
			strcat(tree->code, "=");
			//printf("three %s\n", tree->node1->node1->variable);
			strcat(tree->code, tree->node1->node1->variable);
			//printf("four %s\n", tree->code);
			strcat(tree->code, "*");
			//printf("five %s\n", tree->node2->variable);
			if(tree->node2->node1->variable!=NULL)
				strcat(tree->code, tree->node2->node1->variable);
			else
				strcat(tree->code, tree->node2->variable);
			strcat(tree->code, ";\n");
		//	printf("%s\n", tree->code);
		}


		if(strcmp(tree->data, "/")==0){
			strcpy(tree->code, tree->node1->code);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
		//	printf("%s\n", tree->code);
			strcat(tree->code, tree->variable);
		//	printf("%s\n", tree->code);
			strcat(tree->code, "=");
			//printf("%s\n", tree->node1->node1->variable);
			strcat(tree->code, tree->node1->node1->variable);
		//	printf("%s\n", tree->code);
			strcat(tree->code, "/");
			//printf("%s\n", tree->code);
			if(tree->node2->node1->variable)
				strcat(tree->code, tree->node2->node1->variable);
			else
				strcat(tree->code, tree->node2->variable);
				strcat(tree->code, "\n");
			//printf("%s\n", tree->code);
		}

		if(strcmp(tree->data, "<")==0 || strcmp(tree->data, ">")==0 || strcmp(tree->data, ">=")==0 || strcmp(tree->data, "<=")==0 || strcmp(tree->data, "==" )==0 || strcmp(tree->data, "!=")==0){
		    //printf("sal\n");
			 strcpy(tree->code, tree->node1->code);
			strcat(tree->code, "\n");
			strcat(tree->code, tree->node2->code);
			//printf("one %s\n", tree->code);
			strcat(tree->code, tree->variable);
			//printf("two %s\n", tree->code);
			strcat(tree->code, "=");
			//printf("three %s\n", tree->node1->data);
			if(tree->node1->node1->variable)
				strcat(tree->code, tree->node1->node1->variable);
			else
				strcat(tree->code, tree->node1->variable);
			//printf("four %s\n", tree->code);
			strcat(tree->code, tree->data);
			if(tree->node2->node1->variable!=NULL)
				strcat(tree->code, tree->node2->node1->variable);
			else
				strcat(tree->code, tree->node2->variable);
			strcat(tree->code, "\nifz ");
			strcat(tree->code, tree->variable);
			strcat(tree->code, " goto  L");
			//printf("in here %d\n", tree->falseLabel);
			strcat(tree->code, intoa(tree->falseLabel, buffer, 10));
			strcat(tree->code, "\n ");
		}	
	}
}


 void swap(char *x, char *y){
	char t = *x;
	*x=*y;
	*y=t;
}

char* reverse(char* st, int i, int j){
	while(i<j)
		swap(&st[i++], &st[j--]);
	return st;
}


char* intoa(int value, char* buffer, int base)
{

	if(base<2 || base>32)
		return buffer;
	int n = abs(value);
	int i=0;

	while(n){
		int r = n%base;
		if(r>=10)
			buffer[i++]='A' + (r-10);
		else
			buffer[i++]='0' + r;
		n=n/base;
		}

	if(i==0)
		buffer[i++]='0';
	
	if(value<0 && base==10)
		buffer[i++]='-';
	buffer[i]='\0';

	return reverse(buffer, 0, i-1);
	
}



