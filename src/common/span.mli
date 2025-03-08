(* span *)
(* a span is a [start] loc and [end] loc *)
type t

val init : Location.t -> Location.t -> t

(* get [start] loc *)
val start : t -> Location.t

(* get [end] loc *)
val \#end : t -> Location.t

(* merge two spans *)
val merge : t -> t -> t
