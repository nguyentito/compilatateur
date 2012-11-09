type program = decl list
type decl = qmslkfj

type ctype = Void | Int | Char | Struct of string | Union of string

type binop = And | Equal | Different  | Less
             | LessEq  | Greater  | GreaterEq  | Plus
             | Minus  | Star  | Divide  | Modulo  | Or

type expr = Int of Int32.t
          | String of string
          | Ident of string
          | Deref of expr
          | Subfield of expr * string
