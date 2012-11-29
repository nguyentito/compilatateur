type semantic_error
val error_message : semantic_error -> string

exception Error of semantic_error * Ast.location
exception NoMainFunction
exception InvalidMainFunction

val typecheck_program : Ast.program -> unit
