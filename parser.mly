%{
  let rec multi_pointer t = function
    | 0 -> t
    | n -> Ast.Pointer (multi_pointer t (n-1))

  let with_dummy_loc x = (x, (Lexing.dummy_pos, Lexing.dummy_pos))
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
%nonassoc unary
%left Arrow Dot
%nonassoc strong

%start <Ast.program> parse_source_file

%%

with_location(X):
  | x = X { (x, ($startpos, $endpos)) }

(* Grammar copied from the project specification *)

parse_source_file: ds = decl* ; Eof { ds }
 
decl_noloc:
  | x = decl_vars { Ast.DVars x }
  | x = decl_typ  { Ast.DType x }
  | x = decl_fun  { Ast.DFun  x }
decl : d = with_location(decl_noloc) {d}

decl_vars: t = typ ; vars = separated_nonempty_list(Comma, var) ; Semicolon
    { List.map (fun (n, id) -> (multi_pointer t n, id)) vars }

decl_typ: k = type_keyword ; id = Ident ; LCurly fs = decl_vars* RCurly ; Semicolon
    { k (id, List.concat fs) }

decl_fun: tv = typed_var
          LParen args = separated_list(Comma, typed_var) RParen
          b = block
    { let (t, name) = tv in
      { Ast.fun_name = name ;
        Ast.fun_return_type = t ;
        Ast.fun_args = args ;
        Ast.fun_body = b } }

(* dunno why, but without the %inline, the reduction
   type_keyword -> Struct or Union would never be applied *)
%inline type_keyword:
  | Struct { fun (x,y) -> Ast.DStruct (x,y) }
  | Union  { fun (x,y) -> Ast.DUnion  (x,y) }

typ:
  | Void { Ast.Void } | Int { Ast.Int } | Char { Ast.Char }
  | Struct id = Ident { Ast.Struct id }
  | Union  id = Ident { Ast.Union  id }

var: stars = Star* i = Ident { (List.length stars, i) }

typed_var: t=typ v=var { let (n,i) = v in (multi_pointer t n, i) } 

expr_noloc:
  | i = IntV    { Ast.IntV    i  }
  | s = StringV { Ast.StringV s  }
  | id = Ident  { Ast.Var     id }

  | Star e = expr { Ast.Deref e }
  | a = expr LBracket i = expr RBracket %prec strong
        { Ast.Deref ( (Ast.Binop (Ast.Add, a, i)), ($startpos, $endpos)) }

  | s  = expr  Dot  f = Ident { Ast.Subfield (s, f) }
  | sp = expr Arrow f = Ident { Ast.Subfield ((Ast.Deref sp, ($startpos, $endpos)), f) }

  | l = expr Assign r = expr { Ast.Assign (l, r) }

  | f = Ident LParen a = separated_list(Comma, expr) RParen %prec strong
        { Ast.Apply (f, a) }

  | Increment e = expr { Ast.PreInc  e } %prec unary
  | Decrement e = expr { Ast.PreDec  e } %prec unary
  | e = expr Increment { Ast.PostInc e } %prec unary
  | e = expr Decrement { Ast.PostDec e } %prec unary

  | Address e = expr { Ast.Address e  } %prec unary
  | Not     e = expr { Ast.Not e      } %prec unary
  | Plus    e = expr { Ast.Positive e } %prec unary (* it's actually useful for later *)
  | Minus   e = expr { Ast.Negative e } %prec unary

  | Sizeof LParen t = typ stars = Star* RParen %prec strong
        { Ast.Sizeof (multi_pointer t (List.length stars)) }
  | LParen e = expr_noloc RParen { e }

  | x = expr o = op y = expr { Ast.Binop (o, x, y) }

expr:
  | e = with_location(expr_noloc) { e }
    
%inline op:
  | And {Ast.And} | Or {Ast.Or}
  | Equal {Ast.Equal} | Different {Ast.Different}
  | Less {Ast.Less}       | LessEq {Ast.LessEq}
  | Greater {Ast.Greater} | GreaterEq {Ast.GreaterEq}
  | Plus {Ast.Add} | Minus {Ast.Sub} | Star {Ast.Mul}
  | Divide {Ast.Div} | Modulo {Ast.Modulo}

      
instruction_noloc:
  | Semicolon { Ast.EmptyInstr }
  | e = expr Semicolon { Ast.ExecExpr e }

(* Note that with these grammar rules for if-else, the expression
     if(a) if(b) c else d
   is ambiguous (it can be read as either
     if(a) { if(b) c } else d,
   or
     if(a) { if(b) c else d }
   ), and we rely on Menhir preferring shift over reduce to resolve
   the conflict the "right way". (http://en.wikipedia.org/wiki/Dangling_else)
*)
  | If LParen e = expr RParen i1 = instruction
        { Ast.IfThenElse (e, i1, with_dummy_loc Ast.EmptyInstr) }
  | If LParen e = expr RParen i1 = instruction Else i2 = instruction
        { Ast.IfThenElse (e, i1, i2) }

  | While LParen e = expr RParen i = instruction
        { Ast.While (e, i) }
  | For LParen e1 = separated_list(Comma, expr) Semicolon
               e2 = expr? Semicolon
               e3 = separated_list(Comma, expr)
        RParen i = instruction
        { Ast.For (e1, e2, e3, i) }

  | b = block { Ast.Block b }
  | Return e = expr? Semicolon { Ast.Return e }
 
instruction:
  | i = with_location(instruction_noloc) {i}
    
block:
  | LCurly vars = decl_vars* instr_list = instruction* RCurly
       { { Ast.block_locals = List.concat vars ; Ast.block_instrs = instr_list } }

%%
