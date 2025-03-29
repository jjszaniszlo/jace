(* Lexer *)

type t

val init : Code.t -> t

val next : t -> t * Token.t
