%{
  module A = Ast.Raw

  let rec multi_pointer t = function
    | 0 -> t
    | n -> A.Pointer (multi_pointer t (n-1))

  let with_dummy_loc x = (x, (Lexing.dummy_pos, Lexing.dummy_pos))
%}

(*** Tokens, associativity, precedence ***)

%token Eof
%token Char Else For If Int Return Sizeof Struct Union Void While
%token Comma Semicolon
%token Equal Different LessEq Less GreaterEq Greater
%token Assign
%token And Or Not
%token Increment Decrement
%token Plus Minus Star Divide Modulo
%token Arrow Dot Address
%token LParen RParen LBracket RBracket LCurly RCurly
%token <Int32.t> IntV
%token <int list> StringV
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
%left LParen RParen LBracket RBracket Arrow Dot
%nonassoc Else

%start <Ast.Raw.program> parse_source_file 


%%

with_location(X):
  | x = X { (x, ($startpos, $endpos)) }

(*** Grammar copied from the project specification ***)

parse_source_file: ds = decl* ; Eof { ds }
 
decl_noloc:
  | x = decl_vars { A.DVars x }
  | x = decl_typ  { A.DType x }
  | x = decl_fun  { A.DFun  x }
decl : d = with_location(decl_noloc) {d}

decl_vars: t = typ ; vars = separated_nonempty_list(Comma, var) ; Semicolon
    { List.map (fun (n, id) -> (multi_pointer t n, id)) vars }

decl_typ: k = type_keyword ; id = Ident ; LCurly fs = decl_vars* RCurly ; Semicolon
    { (k, id, List.concat fs) }

decl_fun: tv = typed_var
          LParen args = separated_list(Comma, typed_var) RParen
          b = block
    { let (t, name) = tv in
      { A.fun_name = name ;
        A.fun_return_type = t ;
        A.fun_args = args ;
        A.fun_body = b } }

(* dunno why, but without the %inline, the reduction
   type_keyword -> Struct or Union would never be applied *)
%inline type_keyword:
  | Struct { A.Struct }
  | Union  { A.Union }

typ:
  | Void { A.Void } | Int { A.Int } | Char { A.Char }
  | k = type_keyword; id = Ident { A.Aggregate (k, id) }

var: stars = Star* i = Ident { (List.length stars, i) }

typed_var: t=typ v=var { let (n,i) = v in (multi_pointer t n, i) } 

expr_noloc:
  | i = IntV    { A.IntV    i  }
  | s = StringV { A.StringV s  }
  | id = Ident  { A.Var     id }

  | Star e = expr { A.Deref e }
  | a = expr LBracket i = expr RBracket
        { A.Deref ( (A.Binop (A.Add, a, i)), ($startpos, $endpos)) }

  | s  = expr  Dot  f = Ident { A.Subfield (s, f) }
  | sp = expr Arrow f = Ident { A.Subfield ((A.Deref sp, ($startpos, $endpos)), f) }

  | l = expr Assign r = expr { A.Assign (l, r) }

  | f = Ident LParen a = separated_list(Comma, expr) RParen
        { A.Apply (f, a) }

  | Increment e = expr { A.Incr (A.PreIncr,  e) } %prec unary
  | Decrement e = expr { A.Incr (A.PreDecr,  e) } %prec unary
  | e = expr Increment { A.Incr (A.PostIncr, e) } %prec unary
  | e = expr Decrement { A.Incr (A.PostDecr, e) } %prec unary

  | Address  e = expr { A.Address e   } %prec unary
  | o = unop e = expr { A.Unop (o, e) } %prec unary
  
  | Sizeof LParen t = typ stars = Star* RParen
        { A.Sizeof (multi_pointer t (List.length stars)) }
  | LParen e = expr_noloc RParen { e }

  | x = expr o = binop y = expr { A.Binop (o, x, y) }

expr:
  | e = with_location(expr_noloc) { e }

%inline unop:
  | Not { A.Not }
  | Plus  { A.Positive }
  | Minus { A.Negative }
    
%inline binop:
  | And {A.And} | Or {A.Or}
  | Equal {A.Equal} | Different {A.Different}
  | Less {A.Less}       | LessEq {A.LessEq}
  | Greater {A.Greater} | GreaterEq {A.GreaterEq}
  | Plus {A.Add} | Minus {A.Sub} | Star {A.Mul}
  | Divide {A.Div} | Modulo {A.Modulo}


instruction_noloc:
  | Semicolon { A.EmptyInstr }
  | e = expr Semicolon { A.ExecExpr e }

(* Note that with these grammar rules for if-else, the expression
     if(a) if(b) c else d
   is ambiguous (it can be read as either
     if(a) { if(b) c } else d,
   or
     if(a) { if(b) c else d }
   By setting an appropriate precedence level for the token Else,
   we resolve this conflict in an unambiguous way.
   (http://en.wikipedia.org/wiki/Dangling_else)
*)
  | If LParen e = expr RParen i1 = instruction
        { A.IfThenElse (e, i1, with_dummy_loc A.EmptyInstr) }
  | If LParen e = expr RParen i1 = instruction Else i2 = instruction
        { A.IfThenElse (e, i1, i2) }

  | While LParen e = expr RParen i = instruction
        { A.While (e, i) }
  | For LParen e1 = separated_list(Comma, expr) Semicolon
               e2 = expr? Semicolon
               e3 = separated_list(Comma, expr)
        RParen i = instruction
        { A.For (e1, e2, e3, i) }

  | b = block { A.Block b }
  | Return e = expr? Semicolon { A.Return e }
 
instruction:
  | i = with_location(instruction_noloc) {i}
    
block:
  | LCurly vars = decl_vars* instr_list = instruction* RCurly
       { { A.block_locals = List.concat vars ;
           A.block_instrs = instr_list }
       }

%%
