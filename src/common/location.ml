type t = 
  { line: int
  ; col: int
  ; off: int
  }

let init line col off = { line; col; off; }

let line loc = loc.line

let col loc = loc.col

let off loc = loc.off

