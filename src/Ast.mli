type location = Lexing.position * Lexing.position

type ctype = Void | Int | Char | Struct of string | Union of string
           | Pointer of ctype
           | TypeNull (* Used only for type-checking *)

type binop = And | Or | Equal | Different
           | Less | LessEq  | Greater | GreaterEq
           | Add | Sub | Mul | Div | Modulo

(* Note : the X_noloc types have misleading names
   they have no location info for their root node,
   but the child nodes do have this info *)

type expr_noloc = IntV of Int32.t | StringV of string | Var of string
          | Deref of expr
          | Subfield of expr * string
          | Assign of expr * expr
          | Apply of string * expr list (* No crazy sh** with function pointers allowed *)
          | PreInc of expr | PreDec of expr | PostInc of expr | PostDec of expr
          | Address of expr
          | Not of expr | Positive of expr | Negative of expr
          | Binop of binop * expr * expr
          | Sizeof of ctype

and expr = expr_noloc * location

type decl_var = ctype * string

type instr_noloc = EmptyInstr
           | ExecExpr of expr
           | IfThenElse of expr * instr * instr
           | While of expr * instr 
           | For of expr list * expr option * expr list * instr (* Il vaut mieux que le for(;;)
                                                                   soit désucré après typage *)
           | Block of block
           | Return of expr option
and instr = instr_noloc * location

and block = { block_locals : decl_var list ;
              block_instrs : instr list }

type decl_typ = DStruct of string * decl_var list
              | DUnion of string * decl_var list

type decl_fun = { fun_name : string ;
                  fun_return_type : ctype ;
                  fun_args : decl_var list ;
                  fun_body : block }

type decl_noloc = DVars of decl_var list
                | DType of decl_typ
                | DFun  of decl_fun
and decl = decl_noloc * location

type program = decl list
