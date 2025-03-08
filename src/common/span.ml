type t = { st: Location.t; ed: Location.t }

let init st ed = { st; ed; }

let start span = span.st

let \#end span = span.ed

let merge first second = { st = start first; ed = \#end second }
