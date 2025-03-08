(* source code *)
type t

(* init source code with [data] directly *)
val init : string -> t

(* init source code from [file] *)
val init_from_file : string -> t

(* get [length] of underlying data of source code *)
val length : t -> int

(* peek char at [offset] *)
val peek : t -> int -> char option

(* retrieve [data] in a [span] of source code *)
val read : t -> Span.t -> string
