%{
  (* Transforme le type t en pointeur d'ordre n sur t *)
  let rec multi_pointer t = function
    | 0 -> t
    | n -> Ast.Pointer (multi_pointer t (n-1))

  let for_loop init maybe_cond modif body =
    let group instr_list = Ast.Block ([], instr_list) in
    let exec = List.map (fun expr -> Ast.ExecExpr expr) in
    let cond = match maybe_cond with
      | Some c -> c
      | None   -> Ast.IntV (Int32.of_int 1) in
    group (exec init @ [Ast.While (cond, group (body :: exec modif))])
  
  let make_program decl_list =
    let f (types, globals, funs) = function
      | Ast.DVars vars -> (types,        vars @ globals, funs      )
      | Ast.DType typ  -> (typ :: types, globals,        funs      )
      | Ast.DFun  fn   -> (types,        globals,        fn :: funs)
    in
    let (types, globals, funs) = List.fold_left f ([],[],[]) decl_list in
    { Ast.prog_types   = List.rev types ;
      Ast.prog_globals = List.rev globals ;
      Ast.prog_funs    = List.rev funs }
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

(* Grammar copied from the project specification *)

parse_source_file: ds = decl* ; Eof { make_program ds }
 
decl:
  | x = decl_vars { Ast.DVars x }
  | x = decl_typ  { Ast.DType x }
  | x = decl_fun  { Ast.DFun  x }

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


expr:
  | i = IntV    { Ast.IntV    i  }
  | s = StringV { Ast.StringV s  }
  | id = Ident  { Ast.Ident   id }

  | Star e = expr { Ast.Deref e }
  | a = expr LBracket i = expr RBracket %prec strong
        { Ast.Deref (Ast.Binop (Ast.Add, a, i)) }

  | s  = expr  Dot  f = Ident { Ast.Subfield (s, f) }
  | sp = expr Arrow f = Ident { Ast.Subfield (Ast.Deref sp, f) }

  | l = expr Assign r = expr { Ast.Assign (l, r) }

  | f = Ident LParen a = separated_list(Comma, expr) RParen %prec strong
        { Ast.Apply (f, a) }

  | Increment e = expr { Ast.PreInc  e } %prec unary
  | Decrement e = expr { Ast.PreDec  e } %prec unary
  | e = expr Increment { Ast.PostInc e } %prec unary
  | e = expr Decrement { Ast.PostDec e } %prec unary

  | Address e = expr { Ast.Address e } %prec unary
  | Not e = expr { Ast.Not e } %prec unary
  | Plus e = expr { e } %prec unary
  | Minus e = expr { Ast.Negation e } %prec unary

  | Sizeof LParen t = typ stars = Star* RParen %prec strong
        { Ast.Sizeof (multi_pointer t (List.length stars)) }
  | LParen e = expr RParen { e }

  | x = expr o = op y = expr { Ast.Binop (o, x, y) }
    
%inline op:
  | And {Ast.And} | Or {Ast.Or}
  | Equal {Ast.Equal} | Different {Ast.Different}
  | Less {Ast.Less}       | LessEq {Ast.LessEq}
  | Greater {Ast.Greater} | GreaterEq {Ast.GreaterEq}
  | Plus {Ast.Add} | Minus {Ast.Sub} | Star {Ast.Mul}
  | Divide {Ast.Div} | Modulo {Ast.Modulo}

      
instruction:
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
        { Ast.IfThenElse (e, i1, Ast.EmptyInstr) }
  | If LParen e = expr RParen i1 = instruction Else i2 = instruction
        { Ast.IfThenElse (e, i1, i2) }

  | While LParen e = expr RParen i = instruction
        { Ast.While (e, i) }
  | For LParen e1 = separated_list(Comma, expr) Semicolon
               e2 = expr? Semicolon
               e3 = separated_list(Comma, expr)
        RParen i = instruction
        { for_loop e1 e2 e3 i }

  | b = block { b }
  | Return e = expr? Semicolon { Ast.Return e }
    
block:
  | LCurly vars = decl_vars* instr_list = instruction* RCurly
       { Ast.Block (List.concat vars, instr_list) }

%%
