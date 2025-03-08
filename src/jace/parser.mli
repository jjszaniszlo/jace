
type t

val init : Lexer.t -> t

val parse : t -> Ast.jace_module
