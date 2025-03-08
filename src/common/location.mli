(* location *)
(* expected to contain a [line], [col] and [offset] *)
type t

(* init a location with [line], [col] and [offset] *)
val init : int -> int -> int -> t

(* get location [line] *) 
val line : t -> int

(* get location [col] *) 
val col : t -> int

(* get location [offset] *) 
val off : t -> int
