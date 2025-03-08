(* Lexer *)

type t

val init : Code.t -> t

val next : t -> t * Token.t

val code : t -> Code.t
