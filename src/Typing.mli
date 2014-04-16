exception Error of TypeError.semantic_error * Ast.Raw.location
exception NoMainFunction
exception InvalidMainFunction

val typecheck_program : Ast.Raw.program -> Ast.Typed.program

