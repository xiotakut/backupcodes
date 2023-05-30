%{
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include "lexer.tab.h"

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int yylineno;
extern int yycolumn;

void yyerror(const char* s);

enum Type { Integer, Array, Void };

struct Symbol {
  std::string name;
  Type type;
};

struct Function {
  std::string name;
  std::vector<Symbol> declarations;
};

std::vector <Function> symbol_table;

// remember that Bison is a bottom up parser: that it parses leaf nodes first before
// parsing the parent nodes. So control flow begins at the leaf grammar nodes
// and propagates up to the parents.
Function *get_function() {
  int last = symbol_table.size()-1;
  if (last < 0) {
    printf("***Error. Attempt to call get_function with an empty symbol table\n");
    printf("Create a 'Function' object using 'add_function_to_symbol_table' before\n");
    printf("calling 'find' or 'add_variable_to_symbol_table'");
    exit(1);
  }
  return &symbol_table[last];
}

// find a particular variable using the symbol table.
// grab the most recent function, and linear search to
// find the symbol you are looking for.
// you may want to extend "find" to handle different types of "Integer" vs "Array"
bool find(std::string &value) {
  Function *f = get_function();
  for(int i=0; i < f->declarations.size(); i++) {
    Symbol *s = &f->declarations[i];
    if (s->name == value) {
      return true;
    }
  }
  yyerror(("Variable " + value + " was not declared").c_str());
  return false;
}

// when you see a function declaration inside the grammar, add
// the function name to the symbol table

//Trying to name a variable with the same name as a reserved keyword:
std::set<std::string> keywords = {"IF", "THEN", "ELSE", "WHILE", "RETURN", "ARRAY", "INT", "TRUE", "FALSE", "BREAK", "CONT", "READ", "WRITE", "VOID"};

void add_function_to_symbol_table(std::string &value) {

   if(keywords.find(value) != keywords.end()) {
    yyerror(("Cannot name a function with reserved keyword: " + value).c_str());
    return;
  }

  Function f;
  f.name = value;
  symbol_table.push_back(f);
}

// when you see a symbol declaration inside the grammar, add
// the symbol name as well as some type information to the symbol table
void add_variable_to_symbol_table(std::string &value, Type t) {

  if(keywords.find(value) != keywords.end()) {
    yyerror(("Cannot name a variable with reserved keyword: " + value).c_str());
    return;
  }

  //check the valie deinfing more than once
   Function *f = get_function();
   for (const auto& s : f->declarations) {
    if (s.name == value) {
      yyerror(("Variable " + value + " is already declared").c_str());
      return;
    }
  }

  Symbol s;
  s.name = value;
  s.type = t;
  Function *f = get_function();
  f->declarations.push_back(s);
}

// a function to print out the symbol table to the screen
// largely for debugging purposes.
void print_symbol_table(void) {
  printf("symbol table:\n");
  printf("--------------------\n");
  for(int i=0; i<symbol_table.size(); i++) {
    printf("function: %s\n", symbol_table[i].name.c_str());
    for(int j=0; j<symbol_table[i].declarations.size(); j++) {
      printf("  locals: %s\n", symbol_table[i].declarations[j].name.c_str());
    }
  }
  printf("--------------------\n");
}

//helper function to create temp_variables
std::string create_temp(){
	static int num = 0;
	std::string value = "_temp" + std::to_string(num);
	num+= 1;
	return value;
}

//helper function to create declaration code for a variable
std::string decl_temp_code(std::string &temp){
	return std::string(". ") + temp + std::string("\n");
}

struct CodeNode {
    std::string code; // generated code as a string.
    std::string name;
};

struct DeclNode {
    Type type;
    std::string name;
};

%}

%union {
  char *op_val;
  struct CodeNode *node;
  Type type1;
}

%start program

%token IF THEN ELSE WHILE RETURN ARRAY INT TRUE FALSE BREAK CONT READ WRITE
%token ADD SUB MULT DIV MOD ASSIGN AND NOT EQUAL INEQUAL LT_OR_EQ GT_OR_EQ GT LT LBR RBR LCB RCB LPR RPR SEMCOL COMMA
%token <op_val> IDENT NUM
%token VOID 

%type <node> functions
//todo: add more type <node>s, for params, statements, etc.
%type <type1> type
%type <node> statement
%type <node> statement_list
%type <node> assignment_statement
%type <node> call_statement
%type <node> while_statement
%type <node> break_statement
%type <node> if_statement
%type <node> continue_statement
%type <node> read_statement
%type <node> write_statement
%type <node> declaration_statement
%type <node> block_statement
%type <node> return_statement
%type <node> program
%type <node> params
%type <node> param_list
%type <node> function_definition
%type <node> call_param_list
%type <node> call_params
%type <node> term
%type <node> factor
%type <node> comparison_expression
%type <node> expression

%%

program:
   %empty { // this happens last.
                CodeNode *node = new CodeNode;
                $$ = node;
                printf("Generated code:\n");
                printf("%s\n", code.c_str());

                bool mainFunctionDefined = false;
                for(const auto& f : symbol_table) {
                if(f.name == "main") {
                mainFunctionDefined = true;
                break;
      }
    }
    if(!mainFunctionDefined) {
      yyerror("No main function defined");
      exit(1);
    }
         }
  | functions {
   // this happens last.
       CodeNode *node = $1;
       std::string code = node->code;
       printf("Generated code:\n");
       printf("%s\n", code.c_str());
       $$ = node;

       // Check main function here as well
        bool mainFunctionDefined = false;
        for(const auto& f : symbol_table) {
        if(f.name == "main") {
        mainFunctionDefined = true;
        break;
        }
      }
      if(!mainFunctionDefined) {
      yyerror("No main function defined");
      exit(1);
    }
  }
  ;

functions:
  function_definition {
      CodeNode *func  = $1;
      std::string code = func->code;
      CodeNode *node = new CodeNode;
      node->code = code;
      $$ = node;
   }
  | function_definition functions {
     CodeNode *func  = $1;
     CodeNode *funcs = $2;
     std::string code = func->code + funcs->code;
     CodeNode *node = new CodeNode;
     node->code = code;
     $$ = node;
  }
  ;

function_definition:
  type IDENT LPR params RPR block_statement {
     std::string func_name = $2;
     CodeNode *params = $4;
     CodeNode *stmts  = $6;
     std::string code = std::string("func ") + func_name + std::string("\n");
     Type t = $1;
     add_variable_to_symbol_table(func_name, t);
     code += params->code;
     code += stmts->code;
     code += std::string("endfunc\n");

     CodeNode *node = new CodeNode;
     node->code = code;
     $$ = node;
  }
  ;

params:
  %empty {
  	CodeNode *node = new CodeNode;
  	$$ = node;
  }
  | param_list {
  	CodeNode *node = new CodeNode;
        $$ = $1;
  }
  ;

param_list:
  type IDENT {
  	Type t = $1;
  	std::string value = $2;
  	add_variable_to_symbol_table(value, t);
    std::string code = std::string("param ") + value + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    $$ = node;

  }
  | type IDENT COMMA param_list {
  	Type t = $1;
    std::string value = $2;
    CodeNode *list = $4;
    add_variable_to_symbol_table(value, t);
    std::string code = std::string("param ") + value + std::string("\n") + list->code;
    CodeNode *node = new CodeNode;
    node->code = code;
    $$ = node;
  }
  ;

type:
  INT {
  	Type *t = Integer;
  	$$ = t;
  }
  | VOID {
  	Type *t = Void;
  	$$ = t;
  }
  ;


statement_list:
  %empty { printf("statement_list -> epsilon\n");
  	CodeNode *node = new CodeNode;
    $$ = node;
  }
  | statement SEMCOL statement_list {
	CodeNode *child1 = $1;
	CodeNode *child2 = $3;
	CodeNode *node = new CodeNode;

	std::string code = child1->code + child2->code;
	node->code = code;
	$$ = node;
  }
  | statement statement_list{
	CodeNode *child1 = $1;
    CodeNode *child2 = $2;
    CodeNode *node = new CodeNode;

    std::string code = child1->code + child2->code;
    node->code = code;
    $$ = node;
  }
  ;

statement:
 assignment_statement {
	$$ = $1;
 }
  | call_statement SEMCOL {
	$$ = $1;
  }
  | if_statement {
	$$ = $1;
  }
  | while_statement {
	$$ = $1;
  }
  | break_statement {
	$$ = $1;
  }
  | continue_statement {
	$$ = $1;
  }
  | read_statement {
	$$ = $1;
  }
  | write_statement {
	$$ = $1;
  }
  | declaration_statement {
	$$ = $1;
  }
  | block_statement {
	$$ = $1;
  }
  | return_statement {
	$$ = $1;
  }
  ;

call_statement:
  IDENT LPR call_params RPR {
  	std::string ident = $1;
    CodeNode *params = $3;

    if (!find(ident)) {
      	std::string message = std::string("unidentified symbol '") + ident + std::string("'");
        yyerror(message.c_str());
        return;
    }

	std::string temp = create_temp();
    std::string code = decl_temp_code(temp) + params->code;
    code += std::string("call ") + ident + std::string(", ") + temp;
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  ;

call_params:
  %empty {
	CodeNode *node = new CodeNode;
    $$ = node;
  }
  | call_param_list {
	$$ = $1;
  }
  ;

call_param_list:
  expression {
	$$ = $1;
  }
  | expression COMMA call_param_list {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string code = left->code + right->code;
	CodeNode *node = new CodeNode;
	node->code = code;
	$$ = node;
  }
  ;

assignment_statement:
  IDENT ASSIGN expression {
  	// a := 100 => = a, 100
    // a := b   => = a, b
    std::string ident = $1;
    CodeNode *expression = $3;

    if (!find(ident)) {
    	std::string message = std::string("unidentified symbol '") + ident + std::string("'");
        yyerror(message.c_str());
        return;
    }

     // array index specification error
     Symbol* s = find(ident);
    if(s->type == Array){
        std::string message = std::string("Trying to use array '") + ident + std::string("' as a regular integer");
        yyerror(message.c_str());
    }

    std::string code = expression->code + std::string("= ") + ident + std::string(", ") + expression->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = ident;
    $$ = node;
  }
  | IDENT LBR expression RBR ASSIGN expression { //array item assignment
  	std::string array_name = $1;
  	if (!find(array_name)) {
      	std::string message = std::string("unidentified symbol '") + array_name + std::string("'");
        yyerror(message.c_str());
    }

    //array index specification error
     Symbol* s = find(array_name);
    if(s->type != Array){
        std::string message = std::string("Trying to use regular integer '") + array_name + std::string("' as an array");
        yyerror(message.c_str());
    }

    CodeNode *src = $6;
    CodeNode *index = $3;
    std::string code = index->code + src->code;
    code += std::string("[]= ") + array_name + std::string(", ") + src->name + std::string(", ") + index->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    $$ = node;

  }
  ;

if_statement:
  IF LPR comparison_expression RPR block_statement {
	CodeNode *cmp = $3;
	CodeNode *on_true = $5;

	std::string code = cmp->code + on_true->code;
	//code =

  }
  | IF LPR comparison_expression RPR block_statement ELSE block_statement {

  }
  ;

while_statement:
  WHILE LPR comparison_expression RPR block_statement { printf("while_statement -> WHILE LPR comparison_expression RPR block_statement\n");

  }
  ;

comparison_expression:
  expression LT expression {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string temp = create_temp();

	std::string code = left->code + right->code + decl_temp_code(temp);
	code += std::string("< ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | expression GT expression {
	CodeNode *left = $1;
    CodeNode *right = $3;
    std::string temp = create_temp();

    std::string code = left->code + right->code + decl_temp_code(temp);
    code += std::string("> ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | expression LT_OR_EQ expression {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string temp = create_temp();

	std::string code = left->code + right->code + decl_temp_code(temp);
	code += std::string("<= ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | expression GT_OR_EQ expression {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string temp = create_temp();

	std::string code = left->code + right->code + decl_temp_code(temp);
	code += std::string(">= ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | expression EQUAL expression {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string temp = create_temp();

	std::string code = left->code + right->code + decl_temp_code(temp);
	code += std::string("== ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | expression INEQUAL expression {
	CodeNode *left = $1;
	CodeNode *right = $3;
	std::string temp = create_temp();

	std::string code = left->code + right->code + decl_temp_code(temp);
	code += std::string("!= ") + temp + std::string(", ") + left->name + std::string(", ") + right->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | expression {
	CodeNode *exp = $1;
	std::string name = create_temp();

    std::string code = exp->code + decl_temp_code(name);
    code += std::string("!= ") + name + std::string(", ") + exp->name + std::string(", ") + std::string("0\n");	//todo: not sure if this works
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = name;
    $$ = node;
  }
  ;

break_statement:
  BREAK{ printf("break_statement -> BREAK\n");
	//todo: break is not a MIL code line
  }
  ;

continue_statement:
  CONT { printf("continue_statement -> CONT\n");
	//todo: continue is not a MIL code line
  }
  ;

read_statement:
  READ LPR IDENT RPR {
	CodeNode *node = new CodeNode;
	std::string dst_name = $3;

	if (!find(dst_name)) {
        std::string message = std::string("unidentified symbol '") + dst_name + std::string("'");
    	yyerror(message.c_str());
	}

	std::string code = std::string(".< ") + dst_name + std::string("\n");
	node->code = code;
	node->name = dst_name;
	$$ = node;
  }
  ;

write_statement:
  WRITE LPR expression RPR {
	CodeNode *dst = $3;

	if (!find(dst->name)) {
		std::string message = std::string("unidentified symbol '") + dst->name + std::string("'");
		yyerror(message.c_str());
	}

	std::string code = std::string(".> ") + dst->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = dst->name;
	$$ = node;
  }
  ;

declaration_statement:	//todo: check that %type for statement and declaration_statement and var_list all line up
  INT var_list {
	Type t = Integer;

	CodeNode *node = new CodeNode;	//todo: add code for backtrace? this node needs node->code = varlist->code, but needs to pass type down to var_list

    // check to ensure the size is greater than 0
    for(auto &value: node->var_list){
      if(value.size <= 0){
        std::string message = std::string("Attempting to declare array '") + value.name + std::string("' with size less than or equal to 0");
        yyerror(message.c_str());
      }
      add_variable_to_symbol_table(value.name, t);
    }

	$$ = node;

  }
  | INT IDENT LBR NUM RBR {
	Type t = Integer;
	std::string name = $2;
	std::string n = $4;

	CodeNode *node = new CodeNode;
	node->code = std::string(".[] ") + name + std::string(", ") + n + std::string("\n");
	node->name = name;
	$$ = node;
  }
  ;

var_list:
  IDENT { printf("var_list -> IDENT\n");

  }
  | IDENT COMMA var_list { printf("var_list -> IDENT COMMA var_list\n");

  }
  ;

block_statement:
  LCB statement_list RCB {
	$$ = $2;
  }
  ;

return_statement:
  RETURN expression {
	CodeNode *node = new CodeNode;
	CodeNode* exp = $2;
	std::string code = exp->code + std::string("ret ") + exp->name + std::string("\n");
	node->code = code;
	$$ = node;
  }
  ;

expression:
  expression ADD term {
    CodeNode *expression = $1;
    CodeNode *term = $3;
    std::string temp = create_temp();
    std::string code = expression->code + term->code + decl_temp_code(temp);
    code += std::string("+ ") + temp + std::string(", ") + expression->name + std::string(", ")
              + term->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | call_statement {
	$$ = $1;
  }
  | expression SUB term {
    CodeNode *expression = $1;
    CodeNode *term = $3;
    std::string temp = create_temp();
    std::string code = expression->code + term->code + decl_temp_code(temp);
    code += std::string("- ") + temp + std::string(", ") + expression->name + std::string(", ")
              + term->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | term {
	$$ = $1;
  }
  ;

term:
  term MULT factor {
    CodeNode *term = $1;
    CodeNode *factor = $3;
    std::string temp = create_temp();
    std::string code = term->code + factor->code + decl_temp_code(temp);
    code += std::string("* ") + temp + std::string(", ") + term->name + std::string(", ")
              + factor->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | term DIV factor {
    CodeNode *term = $1;
    CodeNode *factor = $3;
    std::string temp = create_temp();
    std::string code = term->code + factor->code + decl_temp_code(temp);
    code += std::string("/ ") + temp + std::string(", ") + term->name + std::string(", ")
              + factor->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | term MOD factor {
    CodeNode *term = $1;
    CodeNode *factor = $3;
    std::string temp = create_temp();
    std::string code = term->code + factor->code + decl_temp_code(temp);
    code += std::string("% ") + temp + std::string(", ") + term->name + std::string(", ")
              + factor->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | factor {
	$$ = $1;
  }
  ;

factor:
  NUM {
	CodeNode *node = new CodeNode;
	std::string value = $1;
	std::string temp = create_temp();
	std::string code = decl_temp_code(temp);
	code += std::string("= ") + temp + std::string(", ") + value;	//todo: not sure if this works for any x.xx number
	node->code = code;
	node->name = temp;
	$$ = node;
  }
  | IDENT {
  	std::string ident = $1;
  	if (!find(ident)) {
          	std::string message = std::string("unidentified symbol '") + ident + std::string("'");
            yyerror(message.c_str());
    }

	CodeNode *node = new CodeNode;
	node->name = ident;
	$$ = node;
  }
  | IDENT LBR expression RBR { //array element
  	std::string arr_name = $1;
  	if (!find(arr_name)) {
              	std::string message = std::string("unidentified symbol '") + arr_name + std::string("'");
                yyerror(message.c_str());
        }

  	CodeNode *index_exp = $3;
  	std::string temp = create_temp();
  	std::string code = index_exp->code + decl_temp_code(temp);
  	code += std::string("=[] ") + temp + std::string(", ") + arr_name + std::string(", ") + index_exp->name + std::string("\n");
  	CodeNode *node = new CodeNode;
  	node->code = code;
  	node->name = temp;
  	$$ = node;
  }
  | LPR expression RPR {
	$$ = $2;
  }
  | ADD factor {
	$$ = $2;
  }
  | SUB factor {
    CodeNode *factor = $2;
    std::string temp = create_temp();
    std::string code = factor->code + decl_temp_code(temp);
    code += std::string("- ") + temp + std::string(", ") + std::string("0") + std::string(", ")//todo: not sure if immediate 0 works here
              + factor->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  | NOT factor { printf("factor -> NOT factor\n");
    CodeNode *factor = $2;
    std::string temp = create_temp();
    std::string code = factor->code + decl_temp_code(temp);
    code += std::string("! ") + temp + std::string(", ") + factor->name + std::string("\n");	//todo: double check
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
  }
  ;

%%

void yyerror(const char* s) {
  fprintf(stderr, "Error: %s at line %d, column %d\n", s, yylineno, yycolumn);
}


int main(int argc, char** argv) {
  if (argc != 2) {
    printf("Usage: %s <input-file>\n", argv[0]);
    return 1;
  }
  FILE* infile = fopen(argv[1], "r");
  if (!infile) {
    printf("Error opening file: %s\n", argv[1]);
    return 1;
  }

  yyin = infile;

  yyparse();
  fclose(infile);

  return 0;
}
