(* source_code *)
type t =
  { _path: string option
  ; data: string
  }

let init str = { _path = None; data = str }

let init_from_file path =
  let file = In_channel.open_text path in
  let data = In_channel.input_all file in
  In_channel.close file;
  { _path = Some path; data }

let peek src off =
  match off < String.length src.data with
    | true -> Some src.data.[off]
    | _ -> None

let length src = String.length src.data

let clamp v l u = max l (min v u)

let read src span =
  let st = clamp (Location.off (Span.start span)) 0 (length src) in
  let len = min (Location.off (Span.\#end span)) (length src) - st in
  String.sub src.data st len 
