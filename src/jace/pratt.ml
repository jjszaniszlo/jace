
module StringMap = Map.Make (String);;

type prefix_bp = int
type infix_bp = int * int
type postfix_bp = int

type t =
  { mutable prefix: prefix_bp StringMap.t
  ; mutable infix: infix_bp StringMap.t
  ; mutable postfix: postfix_bp StringMap.t
  }

let init () = 
  { prefix = StringMap.empty
  ; infix = StringMap.empty
  ; postfix = StringMap.empty 
  }
