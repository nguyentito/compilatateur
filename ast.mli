type ctype = Void | Int | Char | Struct of string | Union of string
           | Pointer of ctype

type binop = And | Equal | Different  | Less
             | LessEq  | Greater  | GreaterEq  | Plus
             | Minus  | Star  | Divide  | Modulo  | Or

type expr = Int of Int32.t | String of string | Ident of string
          | Deref of expr
          | Subfield of expr * string
          | Assign of expr * expr
          | Apply of string * expr list (* No crazy sh** with function pointers allowed *)
          | PreInc of expr | PreDec of expr | PostInc of expr | PostDec of expr
          | Address of expr
          | Not of expr | Minus of expr
          | Binop of binop * expr * expr
          | Sizeof of ctype
              
type instruction = EmptyInstr
                 | ExecExpr of expr
                 | IfThenElse of expr * instr * instr
                 | While of expr * instr
                 | Block of  qqch
                 | Return of expr option

type decl_var = ctype * string
type decl_typ = DStruct of string * decl_var list
              | DUnion of string * decl_var list
type decl_fun = { fun_return_type : ctype ;
                  fun_args : decl_var list ;
                  fun_body : instruction }
type decl = DVars of decl_var list
          | DType of decl_typ
          | DFun of decl_fun

type program = { prog_types : decl_typ list ;
                 prog_globals : decl_var list ;
                 prog_funs : decl_fun list }
