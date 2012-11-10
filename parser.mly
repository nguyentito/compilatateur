%{
  (* Transforme le type t en pointeur d'ordre n sur t *)
  let rec multi_pointer t = function
    | 0 -> t
    | n -> Ast.Pointer (multi_pointer t (n-1))

  let for_loop init cond modif body =
    let group instr_list = Ast.Block ([], instr_list) in
    let exec = List.map (fun expr -> ExecExpr expr)
    group (exec init @ [Ast.While (cond, group (body :: exec modif))])
  
  let make_program decl_list =
    let f (types, globals, funs) = function
      | DVars vars -> (types, vars @ globals, funs)
      | DType typ -> (typ :: types, vars, funs)
      | DFun  fn -> (types, vars, fn :: funs)
    in
    let (types, globals, funs) = List.fold_left f ([],[],[]) decl_list in
    { prog_types   = List.rev types ;
      prog_globals = List.rev globals ;
      prog_funs    = List.rev funs }
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
%left Arrow Dot strong

%start <Ast.program> parse_source_file

%%

(* Grammar copied from the project specification *)

parse_source_file: ds = decl* ; Eof { make_program ds }
 
decl:
  | x = decl_vars { DVars x }
  | x = decl_typ  { DType x }
  | x = decl_fun  { DFun  x }

decl_vars: t = typ ; vars = separated_nonempty_list(Comma, var) ; Semicolon
    { List.map (fun (n, id) -> (multi_pointer t n, id)) vars }

decl_typ: k = type_keyword ; id = Ident ; LCurly fs = decl_vars* RCurly
    { k (id, List.concat fs) }

decl_fun: tv = typed_var
          LParen args = separated_list(Comma, typed_var) RParen
          b = block
    { let (t, name) = tv in
      { fun_name = name ;
        fun_return_type = t ;
        fun_args = args ;
        fun_body = b } }

type_keyword:
  | Struct { fun x -> DStruct x }
  | Union  { fun x -> DUnion  x }

typ:
  | Void { Ast.Void } | Int { Ast.Int } | Char { Ast.Char }
  | Struct id = Ident { Ast.Struct id }
  | Union  id = Ident { Ast.Union  id }

var: stars = Star* i = Ident { (List.length stars, i) }

typed_var: t=typ v=var { let (n,i) = v in (multi_pointer t n, i) } 


expr:
  | i = IntV    { Ast.Int    i  }
  | s = StringV { Ast.String s  }
  | id = Ident  { Ast.Ident  id }

  | Star e = expr { Ast.Deref e }
  | a = expr LBracket i = expr RBracket %prec strong
        { Ast.Deref (Ast.Binop (Ast.Plus, a, i)) }

  | s  = expr  Dot  f = Ident { Ast.Subfield (s, f) }
  | sp = expr Arrow f = Ident { Ast.Subfield (Ast.Deref sp, f) }

  | l = expr Assign r = expr { Ast.Assign (l, r) }

  | f = Ident LParen a = separated_list(Comma, expr) RParen %prec strong
        { Ast.Apply f a }

  | Increment e = expr { Ast.PreInc  e } %prec unary
  | Decrement e = expr { Ast.PreDec  e } %prec unary
  | e = expr Increment { Ast.PostInc e } %prec unary
  | e = expr Decrement { Ast.PostDec e } %prec unary

  | Address e = expr { Ast.Address e } %prec unary
  | Not e = expr { Ast.Not e } %prec unary
  | Plus e = expr { e } %prec unary
  | Minus e = expr { Ast.Minus e } %prec unary

  | Sizeof LParen t = typ stars = Star* RParen %prec strong
        { Ast.Sizeof (multi_pointer (List.length stars) t) }
  | LParen e = expr RParen { e }

  | x = expr o = op y = expr { Ast.Binop (o, x, y) }
    
%inline op:
  | And {Ast.And} | Or {Ast.Or} | Equal {Ast.Equal} | Different {Ast.Different}
  | Less {Ast.Less}       | LessEq {Ast.LessEq}
  | Greater {Ast.Greater} | GreaterEq {Ast.GreaterEq}
  | Plus {Ast.Plus} | Minus {Ast.Minus}
  | Star {Ast.Star} | Divide {Ast.Divide} | Modulo {Ast.Modulo}

      
instruction:
  | Semicolon { Ast.EmptyInstr }
  | e = expr Semicolon { Ast.ExecExpr expr }

  | If LParen e = expr RParen i1 = instruction
        { Ast.IfThenElse e i1 Ast.EmptyInstr }
  | If LParen e = expr RParen i1 = instruction Else i2 = instruction
        { Ast.IfThenElse e i1 i2 }
  | While LParen e = expr RParen i = instruction
        { Ast.While e i }

  | For LParen e1 = separated_list(Comma, expr) Semicolon
               e2 = expr? Semicolon
               e3 = separated_list(Comma, expr)
        RParen i = instruction
        { for_loop e1 e2 e3 i }

  | b = block { b }
  | Return e = expr? Semicolon { Ast.Return e }
    
block:
  | LCurly vars = decl_vars* instr_list = instruction* RCurly
       { Ast.Block (List.concat decl_vars, instr_list) }

%%
