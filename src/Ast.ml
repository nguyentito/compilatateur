module SMap = Map.Make(String)

module Common = struct
  type ctype = Void | Int | Char | Struct of string | Union of string
             | Pointer of ctype
             | TypeNull (* Used only for type-checking *)

  type binop = And | Or | Equal | Different
             | Less | LessEq  | Greater | GreaterEq
             | Add | Sub | Mul | Div | Modulo
  type unop = Not | Positive | Negative

  type incr = PreIncr | PostIncr | PreDecr | PostDecr
end

module Raw = struct
  include Common

  type location = Lexing.position * Lexing.position

  type expr' = IntV of Int32.t | StringV of int list | Var of string
             | Deref of expr
             | Subfield of expr * string
             | Assign of expr * expr
             (* No crazy sh** with function pointers allowed *)
             | Apply of string * expr list
             | Incr of incr * expr
             | Address of expr
             | Unop of unop * expr
             | Binop of binop * expr * expr
             | Sizeof of ctype

  and expr = expr' * location

  type decl_var = ctype * string

  type instr' = EmptyInstr
              | ExecExpr of expr
              | IfThenElse of expr * instr * instr
              | While of expr * instr 
              | For of expr list * expr option * expr list * instr
              | Block of block
              | Return of expr option
  and instr = instr' * location

  and block = { block_locals : decl_var list ;
                block_instrs : instr list }

  type decl_typ = DStruct of string * decl_var list
                | DUnion of string * decl_var list

  type decl_fun = { fun_name : string ;
                    fun_return_type : ctype ;
                    fun_args : decl_var list ;
                    fun_body : block }

  type decl' = DVars of decl_var list
             | DType of decl_typ
             | DFun  of decl_fun
  and decl = decl' * location

  type program = decl list
end

module Typed = struct
  include Common

  type lvalue = Var of string
              | Deref of expr
              | LSubfield of ctype * lvalue * string

  and expr' = IntV of Int32.t | StringV of int list
            | LValue of lvalue
            | Subfield of expr * string
            | Assign of lvalue * expr
            | Incr of incr * lvalue
            | Apply of string * expr list
            | Address of lvalue
            | Unop of unop * expr
            | Binop of binop * expr * expr
            | Sizeof of ctype
  and expr = ctype * expr'

  type decl_var = ctype * string

  type instr = EmptyInstr
             | ExecExpr of expr
             | IfThenElse of expr * instr * instr
             | While of expr * instr 
             | For of expr list * expr option * expr list * instr
             | Block of block
             | Return of expr option

  and block = { block_locals : decl_var list ;
                block_instrs : instr list }

  type decl_fun = { fun_name : string ;
                    fun_return_type : ctype ;
                    fun_args : decl_var list ;
                    fun_body : block }

  type program = { prog_globals : decl_var list;
                   prog_structs : decl_var list SMap.t;
                   prog_unions  : decl_var list SMap.t;
                   prog_funs : decl_fun list }

end

