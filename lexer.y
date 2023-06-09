%{
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <set>
#include <sstream>
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
  bool param_or_not;
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

  //check if the value is defined more than once
   printf(("adding variable " + value + "\n").c_str());
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
  Function *func = get_function();
  func->declarations.push_back(s);
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
  std::stringstream ss;
  ss << num;
	std::string value = "_temp" + std::to_string(num);
	num+= 1;
	return value;
}

//helper function to create declaration code for a variable
std::string decl_temp_code(std::string &temp){
	return std::string(". ") + temp + std::string("\n");
}

//helper function to create declaration code for a label, uses temp_variables
std::string decl_label_code(std::string &temp){
	return std::string(": ") + temp + std::string("\n");
}




struct CodeNode {
    std::string code; // generated code as a string.
    std::string name;
};

struct DeclNode {
    Type type;
    CodeNode node;
};

%}

%union {
  char *op_val;
  struct CodeNode *node;
  struct DeclNode *decl;
}

%start program

%token IF THEN ELSE WHILE RETURN ARRAY INT TRUE FALSE BREAK CONT READ WRITE
%token ADD SUB MULT DIV MOD ASSIGN AND NOT EQUAL INEQUAL LT_OR_EQ GT_OR_EQ GT LT LBR RBR LCB RCB LPR RPR SEMCOL COMMA
%token <op_val> IDENT NUM
%token VOID 

%type <node> functions
%type <decl> type //todo: add more type <node>s, for params, statements, etc.
%type <decl> var_list
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
   printf("empty here");
                CodeNode *node = new CodeNode;
                $$ = node;
		std::string code = node->code;
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
      $$ = $1;
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
  type IDENT{
    std::string func_name = $2;
    add_function_to_symbol_table(func_name);
  } LPR params RPR block_statement {
     DeclNode *type =$1;
     Type t = type->type;
     std::string func_name = $2;
     CodeNode *params = $5;
     CodeNode *stmts  = $7;
     std::string code = std::string("func ") + func_name + std::string("\n");

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
     DeclNode *type =$1;
     Type t = type->type;
  	
  	std::string value = $2;
  	add_variable_to_symbol_table(value, t);
    std::string code = std::string("param ") + value + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    $$ = node;

  }
  | type IDENT COMMA param_list {
     DeclNode *type =$1;
     Type t = type->type;
    
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
  	DeclNode *node = new DeclNode;
	node->type = Integer;
	CodeNode *temp_node = new CodeNode;
	node->node = *temp_node;
  	$$ = node;
  }
  | VOID {

    DeclNode *node = new DeclNode;
    	node->type = Void;
    	CodeNode *temp_node = new CodeNode;
        node->node = *temp_node;
      	$$ = node;
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

    for(int i=0; i<symbol_table.size(); i++) {
    if( symbol_table[i].name == ident){
	  std::string temp = create_temp();
    std::string code = decl_temp_code(temp) + params->code;
    code += std::string("call ") + ident + std::string(", ") + temp;
    CodeNode *node = new CodeNode;
    node->code = code;
    node->name = temp;
    $$ = node;
    break;
    }
    else if(symbol_table[i].name != ident && i == symbol_table.size()-1){
    std::string message = std::string("unidentified symbol '") + ident + std::string("'");
    yyerror(message.c_str());
    }

    }

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

    //array index specification error	//todo: this does not make sense, the high level code is: a[x + 2] = y + 3;, what is this error?
    //Symbol* s = find(array_name);
    //if(s->type != Array){
    //    std::string message = std::string("Trying to use regular integer '") + array_name + std::string("' as an array");
    //    yyerror(message.c_str());
    //}

    CodeNode *src = $6;
    CodeNode *index = $3;
    std::string code = index->code + src->code;
    code += std::string("[]= ") + array_name + std::string(", ") + index->name + std::string(", ") + src->name + std::string("\n");
    CodeNode *node = new CodeNode;
    node->code = code;
    $$ = node;

  }
  ;

if_statement:
  IF LPR comparison_expression RPR block_statement {
	CodeNode *cmp = $3;
	CodeNode *on_true = $5;

	std::string code = cmp->code;
	std::string temp = create_temp();
	code += decl_temp_code(temp);
	code += std::string("! ") + temp + std::string(", ") + cmp->name + std::string("\n");	//! false, original exp
	std::string on_false = create_temp();
	code += std::string("?:= ") + on_false + std::string(", ") + temp + std::string("\n");	//exit if statement if false
	code += on_true->code;																	//inside on_true code
	code += decl_label_code(on_false);														//exit if statement label
	CodeNode *node = new CodeNode;
	node->code = code;
	$$ = node;

  }
  | IF LPR comparison_expression RPR block_statement ELSE block_statement {
	CodeNode *cmp = $3;
	CodeNode *on_true = $5;
	CodeNode *on_false = $7;

	std::string code = cmp->code;
	std::string temp = create_temp();
	code += decl_temp_code(temp);
	code += std::string("! ") + temp + std::string(", ") + cmp->name + std::string("\n");
	std::string on_false_label = create_temp();
	std::string exit_if = create_temp();

	code += std::string("?:= ") + on_false_label + std::string(", ") + temp + std::string("\n");	//jump to false if false
	code += on_true->code;																	//true code
	code += std::string(":= ") + exit_if + std::string("\n");								//exit if jump
	code += decl_label_code(on_false_label) + on_false->code;										//false label + code
	code += decl_label_code(exit_if);														//exit label

	CodeNode *node = new CodeNode;
	node->code = code;
	$$ = node;
  }
  ;

while_statement:
  WHILE LPR comparison_expression RPR block_statement {
	CodeNode *cmp = $3;
	CodeNode *on_true = $5;

	std::string code = std::string("");
	std::string temp = create_temp();
	std::string return_loop = create_temp();
	code += decl_temp_code(temp);
	code += decl_label_code(return_loop) + cmp->code;										//return/start of loop + comparison code
	code += std::string("! ") + temp + std::string(", ") + cmp->name + std::string("\n");	//todo: check that the cmp re-computes when necessary
	std::string on_false = create_temp();

	code += std::string("?:= ") + on_false + std::string(", ") + temp + std::string("\n");	//exit if false
	code += on_true->code + std::string(":= ") + return_loop + std::string("\n");			//loop code + return to start of loop
	code += decl_label_code(on_false);
	CodeNode *node = new CodeNode;
	node->code = code;
	$$ = node;
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
  BREAK{
  //{ printf("break_statement -> BREAK\n");
	//todo: break is not a MIL code line
  //}
  CodeNode *node = new CodeNode;
  node->code = std::string("break\n");
  $$ = node;
  }
  ;

continue_statement:
  CONT {
    //{ printf("continue_statement -> CONT\n");
	//todo: continue is not a MIL code line
  //}
  CodeNode *node = new CodeNode;
  node->code = std::string("continue\n");
  $$ = node;
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

	std::string code = dst->code + std::string(".> ") + dst->name + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	node->name = dst->name;
	$$ = node;
  }
  ;

declaration_statement:
  INT var_list {
	Type t = Integer;
  DeclNode *list = $2;
  
	CodeNode *node = new CodeNode;
  $2->type = t;
  node ->code = list->node.code;
	


	$$ = node;

  }
  | INT IDENT LBR NUM RBR {
	Type t = Array;
	std::string name = $2;
	std::string n = $4;

        // check to ensure the size is greater than 0
      if(std::stoi(n) < 0){
        std::string message = std::string("Attempting to declare array '") + name + std::string("' with size less than or equal to 0");
        yyerror(message.c_str());
      }
      add_variable_to_symbol_table(name, t);
    
	CodeNode *node = new CodeNode;
	node->code = std::string(".[] ") + name + std::string(", ") + n + std::string("\n");
	node->name = name;
	$$ = node;
  }
  ;

var_list:	//todo: needs error  checking for duplicate variables
  IDENT {

	std::string ident = $1;
	add_variable_to_symbol_table(ident, $$->type);
	std::string code = std::string(". ") + ident + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	DeclNode *decl = new DeclNode;
	decl->type = $$->type;
	decl->node = *node;
	$$ = decl;
  }
  | IDENT COMMA var_list { printf("var_list -> IDENT COMMA var_list\n");
	std::string ident = $1;
	DeclNode *more_vars = $3;
	add_variable_to_symbol_table(ident, $$->type);
	std::string code = more_vars->node.code;
	code += std::string(". ") + ident + std::string("\n");
	CodeNode *node = new CodeNode;
	node->code = code;
	DeclNode *decl = new DeclNode;
	decl->type = $$->type;
	decl->node = *node;
	$$ = decl;
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
	code += std::string("= ") + temp + std::string(", ") + value + std::string("\n");	//todo: not sure if this works for any x.xx number
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
	node->code = std::string("");
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
  | NOT factor {
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
