type t = { code : Code.t; st : Location.t; curr : Location.t }

let init code =
  let st = Location.init 1 1 0 in
  { code; st; curr = st }
;;

let peek lexer off = Code.peek lexer.code (Location.off lexer.curr + off)

let code lexer = lexer.code

(* advance lexer by n *)
let rec advance lexer n =
  if n <= 0 || Location.off lexer.curr >= Code.length lexer.code then
    lexer
  else
    let off = Location.off lexer.curr in
    let line, col =
      if peek lexer 0 = Some '\n' then (Location.line lexer.curr + 1, 0)
      else (Location.line lexer.curr, Location.col lexer.curr + 1)
    in
    advance { lexer with curr = Location.init line col (off + 1) } (n - 1)
;;

(* consume while pred is true for each peeked char and return final string*)
let consume_while lexer pred =
  let rec inner lexer pred =
    match peek lexer 0 with
    | Some ch when pred ch -> inner (advance lexer 1) pred
    | _ -> lexer
  in
  let st = lexer.curr in
  let lexer = inner lexer pred in
  (Code.read lexer.code (Span.init st lexer.curr), lexer)
;;

let token_from lexer kind = Token.{ kind; span = Span.init lexer.st lexer.curr }
let is_alpha ch = match ch with 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_ident_char ch = is_alpha ch || ch = '_'
let is_digit ch = match ch with '0' .. '9' -> true | _ -> false
let is_alphanumeric ch = is_alpha ch || is_digit ch
let is_whitespace ch = match ch with ' ' | '\n' | '\r' | '\t' | '\x0C' -> true | _ -> false

let is_operator ch =
  match ch with
  | '!' | '#' | '$' | '%' | '^' | '&' | '*' | '-' | '+' | '=' | '|'
  | ':' | '<' | '>' | '?' | '/' | '.' ->
      true
  | _ -> false
;;

let is_delimeter ch =
  match ch with
  | ';' | ',' | '(' | ')' | '[' | ']' | '{' | '}' | '\\' -> true
  | _ -> false
;;

let rec next lexer =
  let _, lexer = consume_while lexer is_whitespace in
  let lexer = { lexer with st = lexer.curr } in
  match peek lexer 0 with
  | Some '-'  -> skip_comment lexer
  | Some '"'  -> next_string lexer
  | Some '@'  -> next_primitive_op lexer
  | Some '\'' -> next_generic_identifier lexer
  | Some c when is_ident_char c -> next_identifier lexer
  | Some c when is_digit c -> next_number lexer
  | Some c when is_delimeter c -> next_delimeter lexer
  | Some c when is_operator c -> next_operator lexer
  | None -> advance_with_kind lexer Token.Eof
  | _ -> advance_with_kind lexer Token.Invalid

and advance_with_kind lexer kind =
  let lexer = advance lexer 1 in
  (lexer, token_from lexer kind)

and next_identifier lexer =
  let s, lexer = consume_while lexer is_ident_char in
  let s2, lexer = consume_while lexer is_alphanumeric in
  let kind =
    match s ^ s2 with
    | "type"      -> Token.TypeKeyword
    | "class"     -> Token.ClassKeyword
    | "instance"  -> Token.InstanceKeyword
    | "def"       -> Token.DefKeyword
    | "where"     -> Token.WhereKeyword
    | "let"       -> Token.LetKeyword
    | "in"        -> Token.InKeyword
    | "case"      -> Token.CaseKeyword
    | "if"        -> Token.IfKeyword
    | "then"      -> Token.ThenKeyword
    | "else"      -> Token.ElseKeyword
    | "elseif"    -> Token.ElseIfKeyword
    | "ref"       -> Token.RefKeyword
    | "inherit"   -> Token.InheritKeyword
    | _           -> Token.Identifier
  in
  (lexer, token_from lexer kind)

and next_generic_identifier lexer =
  let _, lexer = consume_while (advance lexer 1) is_ident_char in
  let _, lexer = consume_while lexer is_alphanumeric in
  (lexer, token_from lexer Generic)

and next_number lexer =
  let _, lexer = consume_while lexer is_digit in
  let lexer, is_float =
    if peek lexer 0 = Some '.' then
      let lexer = advance lexer 1 in
      let _, lexer = consume_while lexer is_digit in
      (lexer, true)
    else (lexer, false)
  in
  match is_float with
  | true -> (lexer, token_from lexer Token.Float)
  | false -> (lexer, token_from lexer Token.Integer)

and next_string lexer =
  let open Token in
  let rec inner lexer =
    match (peek lexer 0, peek lexer 1) with
    | Some '\\', Some '\\'
    | Some '\\', Some '"'
    | Some '\\', Some 't'
    | Some '\\', Some 'n'
    | Some '\\', Some 'r' -> inner (advance lexer 2)
    | Some '\\', Some _   ->
      (* handle invalid escape sequences *)
      let lexer, _ = inner (advance lexer 2) in
      (lexer, false)
    | Some '"' , _  -> (advance lexer 1, true)
    | Some '\n', _  -> (lexer, false)
    | Some _   , _  -> inner (advance lexer 1)
    | None     , _  -> (lexer, false)
  in
  let lexer, terminated = inner (advance lexer 1) in
  if terminated then (lexer, token_from lexer String)
  else (lexer, token_from lexer InvalidString)

and next_primitive_op lexer =
  let op, lexer = consume_while (advance lexer 1) is_alpha in
  let kind =
    match "@" ^ op with
    | "@and" -> Token.AndOp   (* '@and' *)
    | "@not" -> Token.NotOp   (* '@not' *)
    | "@or"  -> Token.OrOp    (* '@or' *)

    (* integer primitives *)
    | "@intAdd" -> Token.IntAddOp (* '@intAdd' *)
    | "@intSub" -> Token.IntSubOp (* '@intSub' *)
    | "@intMul" -> Token.IntMulOp (* '@intMul' *)
    | "@intDiv" -> Token.IntDivOp (* '@intDiv' *)
    | "@intExp" -> Token.IntExpOp (* '@intExp' *)
    | "@intNeg" -> Token.IntNegOp (* '@intExp' *)

    (* float primitives *)
    | "@floatAdd" -> Token.FloatAddOp (* '@floatAdd' *)
    | "@floatSub" -> Token.FloatSubOp (* '@floatSub' *)
    | "@floatMul" -> Token.FloatMulOp (* '@floatMul' *)
    | "@floatDiv" -> Token.FloatDivOp (* '@floatDiv' *)
    | "@floatExp" -> Token.FloatExpOp (* '@floatExp' *)
    | "@floatNeg" -> Token.FloatNegOp (* '@intExp' *)

    (* string primitive operators *)
    | "@strLen"     -> Token.StrLenOp    (* '@strLen' *)
    | "@strAppend"  -> Token.StrAppendOp (* '@strAppend' *)
    | "@strConcat"  -> Token.StrConcatOp (* '@strConcat' *)
    | "@strCharAt"  -> Token.StrCharAtOp (* '@strCharAt' *)

    (* list primitive operations *)
    | "@listLen"    -> Token.ListLenOp    (* '@listLen' *)
    | "@listAppend" -> Token.ListAppendOp (* '@listAppend' *)
    | "@listIndex"  -> Token.ListIndexOp  (* '@listIndex' *)

    (* set operators *)
    | "@recordIndex" -> Token.RecordIndexOp   (* '@recordIndex' *)
    | "@recordLen"   -> Token.RecordLenOp     (* '@recordLen' *)

    (* ref and deref *)
    | "@ref"   -> Token.RefOp   (* '@ref' *)
    | "@deref" -> Token.DerefOp (* '@deref' *)
    | _           -> Token.InvalidPrimitiveOp
  in
  lexer, token_from lexer kind
  
and next_delimeter lexer =
  let kind =
    match peek lexer 0 with
    (* delimiters *)
    | Some ';' -> Token.SemiColon
    | Some ',' -> Token.Comma
    | Some '\\'-> Token.BackSlash
    | Some '(' -> Token.LeftParen
    | Some ')' -> Token.RightParen
    | Some '{' -> Token.LeftBrace
    | Some '}' -> Token.RightBrace
    | Some '[' -> Token.LeftBracket
    | Some ']' -> Token.RightBracket
    (* technically unreachable, but idk how to represent that in OCaml, so i just have an invalid token type *)
    | _        -> Token.Invalid
  in
  let lexer = advance lexer 1 in
  (lexer, token_from lexer kind)

and next_operator lexer =
  let op, lexer = consume_while lexer is_operator in
  let kind =
    match op with
    | "="  -> Token.Equal
    | ":=" -> Token.ColonEqual
    | "::" -> Token.ColonColon
    (* fat arrow is a delimeter but it consists of operators.  Its cleaner to put it here *)
    | "=>" -> Token.FatArrow
    | _    -> Token.Operator
  in
  (lexer, token_from lexer kind)

and skip_comment lexer =
  let skip_comment' lexer =
    let _, lexer = consume_while (advance lexer 2) (fun ch -> ch <> '\n') in
    advance lexer 1
  in
  if peek lexer 1 = Some '-' then
    next (skip_comment' lexer)
  else next_operator lexer
;;
