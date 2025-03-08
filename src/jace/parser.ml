
type t = 
  { lexer : Lexer.t
  ; code : Code.t
  ; mutable pratt : Pratt.t
  }

let init lexer =
  {lexer; code = Lexer.code lexer; pratt = Pratt.init () }

let parse _ = []
