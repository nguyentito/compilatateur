%{
  let rec multi_pointer t = function
    | 0 -> t
    | n -> Ast.Pointer (multi_pointer t (n-1))

 let for_loop init cond modif body =
   let fuck = "" and you = 42 in
   Ast.Block (failwith fuck you)
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
%right Not Increment Decrement Address unary
%left Arrow Dot 

%start file

%%

(* Grammar copied from the project specification *)

file: ds = decl* Eof { ds }
 
decl: decl_vars | decl_typ | decl_fct

decl_vars: t = typ ; vars = separated_nonempty_list(Comma, var) { }

decl_typ: kw = (Struct | Union) id = Ident
          LBracket decl_vars* RBracket

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
   | s = expr Dot f = Ident { Ast.Subfield (s, f) }
   | sp = expr Arrow f = Ident { Ast.Subfield (Ast.Deref sp, f) }
   | l = expr Assign r = expr { Assign (l, r) }
   | f = Ident LParen a = separated_list(Comma, expr) RParen
       { Ast.Apply f a }
   | Increment e = expr { Ast.PreInc  e } %prec unary
   | Decrement e = expr { Ast.PreDec  e } %prec unary
   | e = expr Increment { Ast.PostInc e } %prec unary
   | e = expr Decrement { Ast.PostDec e } %prec unary
   | Address e = expr { Ast.Address e } %prec unary
   | Not e = expr { Ast.Not e } %prec unary
   | Plus e = expr { e } %prec unary
   | Minus e = expr { Ast.Minus e } %prec unary
   | x = expr o = op y = expr { Ast.Binop (o, x, y) }
   | Sizeof LParen t = typ stars = Star* RParen
       { Ast.Sizeof (multi_pointer (List.length stars) t) }
   | LParen e = expr RParen { e }

%inline op :
   | And {Ast.And}  | Or {Ast.Or} | Equal {Ast.Equal} | Different {Ast.Different}
   | Less {Ast.Less}       | LessEq {Ast.LessEq}
   | Greater {Ast.Greater} | GreaterEq {Ast.GreaterEq}
   | Plus {Ast.Plus} | Minus {Ast.Minus}
   | Star {Ast.Star} | Divide {Ast.Divide} | Modulo {Ast.Modulo}

instruction:
   | Semicolon
   | e = expr Semicolon { Ast.ExecExpr expr }
   | If LParen e = expr RParen i1 = instr { Ast.IfThenElse e i1 Ast.EmptyInstr }
   | If LParen e = expr RParen i1 = instr Else i2 = instr
       { Ast.IfThenElse e i1 i2 }
   | While LParen e = expr RParen i = instr
       { Ast.While e i }
   | For LParen e1 = separated_list(Comma, expr) Semicolon
                e2 = expr? Semicolon
                e3 = separated_list(Comma, expr)
         RParen i = instr
       { for_loop e1 e2 e3 i }
   | b = block { b }
   | Return e = expr? Semicolon { Ast.Return e }

block:
   | LBracket vars = decl_vars* instr_list = instruction* RBracket
       {  }

%%
