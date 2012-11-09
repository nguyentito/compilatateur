%{

%}

%token Eof
%token Char Else For If Int Return Sizeof Struct Union Void While
%token Comma Semicolon Colon
%token Equal Different LessEq Less GreaterEq Greater
%token Assign
%token And Or Not
%token Increment Decrement
%token Plus Minus Star Divide Modulo
%token Arrow Dot Address
%token LParen RParen LBracket RBracket LCurly RCurly
%token <Int32.t> IntV
%token <string> StringV
%token <string> Ident


%right Assign
%left Or
%left And
%left Equal Different
%left Less LessEq Greater GreaterEq
%left Plus Minus
%left Star Divide Modulo
%right Not Increment Decrement Address 
%left Arrow Dot 

%start file

%%

(* Grammar copied from the project specification *)

file: ds = decl* Eof { decl* }
 
decl: decl_vars | decl_typ | decl_fct

decl_vars: t = typ ; vars = separated_nonempty_list(Comma, var) { }

decl_typ: LParen kw = (Struct | Union) id = Ident LBracket decl_vars*

decl_fct: t=typ Star* id = Ident LParen arg = argument RParen b= block

typ: Void { Ast.Void }
   | Int { Ast.Int }
   | Char { Ast.Char }
   | Struct id = Ident {Ast.Struct id}
   | Union id = Ident {Ast.Union id}

argument : t=typ v=var {}

var: ident | Star var

expr:
   | i = IntV { Ast.Int i }
   | s = StringV { Ast.String s }
   | id = Ident { Ast.Ident id }
   | Star e = expr { Ast.Deref e }
   | a = expr LBracket i = expr RBracket
       { Ast.Deref (Ast.Binop (Ast.Plus, a, i)) }
   | s = expr Dot f = Ident { Subfield (s, f) }
   | expr Arrow Ident
   | expr Assign expr
   | Ident LParen separated_list(Comma, expr) RParen
   | Increment expr | Decrement expr
   | expr Increment | expr Decrement
   | Address expr
   | Not expr | Minus expr | Plus expr
   | x = expr o = op y = expr { Ast.Binop (o, x, y) }
   | Sizeof LParen typ Star* RParen
   | LParen expr RParen

%inline op : And {Ast.And} | Equal {Ast.Equal} | Different {Ast.Different} | Less {Ast.Less}
   |LessEq {Ast.LessEq} |Greater {Ast.Greater} | GreaterEq {Ast.GreaterEq} | Plus {Ast.Plus} 
   |Minus {Ast.Minus} |Star {Ast.Star} | Divide {Ast.Divide} | Modulo {Ast.Modulo} |Or {Ast.Or}

instruction:
   | Semicolon
   | expr Semicolon
   | If LParen expr RParen instr
   | If LParen expr RParen instr Else instr
   | While LParen expr RParen instr
   | For LParen separated_list(Comma, expr) Semicolon expr? Semicolon separated_list(Comma, expr)
         RParen instr
   | block
   | Return expr? Semicolon

block:
   | LBracket decl_vars* instruction* RBracket

%%
