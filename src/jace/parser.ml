type op_precedence =
  { prefix: int Env.t
  ; infix: (int * int) Env.t
  ; postfix: int Env.t
  }

type t = 
  { mutable lexer : Lexer.t
  ; code : Code.t
  ; mutable current_tok : Token.t
  ; mutable next_two_toks : Token.t * Token.t
  ; mutable diagnostics : Diagnostic.t list
  ; mutable error_occured : bool 
  ; ops : op_precedence 
  }

let add_diagnostic parser msg span =
  if parser.error_occured then
    ()
  else 
    parser.error_occured <- true;
    parser.diagnostics <- Diagnostic.{msg; span} :: parser.diagnostics

let init lexer =
  let ops =
    { prefix = Env.from_list
      [ ( "@not", 1 )
      ; ( "@intNeg", 2 )
      ; ( "@floatNeg", 2 )
      ; ( "@strLen", 2 )
      ; ( "@strCharAt", 2 )
      ; ( "@listLen", 2 )
      ; ( "@recordLen", 2 )
      ; ( "@ref", 2 )
      ; ( "@deref", 2 )
      ]
    ; infix = Env.from_list
      [ ( "@and", (1, 2) )
      ; ( "@or", (1, 2) )
      ; ( "@intAdd", (1, 2) )
      ; ( "@intSub", (1, 2) )
      ; ( "@intMul", (1, 2) )
      ; ( "@intDiv", (1, 2) )
      ; ( "@intExp", (1, 2) )

      ; ( "@floatAdd", (1, 2) )
      ; ( "@floatSub", (1, 2) )
      ; ( "@floatMul", (1, 2) )
      ; ( "@floatDiv", (1, 2) )
      ; ( "@floatExp", (1, 2) )

      ; ( "@strAppend", (1, 2) )
      ; ( "@strConcat", (1, 2) )

      ; ( "@listAppend", (1, 2) )
      ; ( "@listIndex", (1, 2) )

      ; ( "@recordIndex", (1, 2) )
      ]
    ; postfix = Env.from_list []
    }
  in
  let lexer, current_tok = Lexer.next lexer in
  let lexer, next_fst = Lexer.next lexer in
  let lexer, next_scd = Lexer.next lexer in
  { lexer
  ; code = Lexer.code lexer
  ; current_tok
  ; next_two_toks = (next_fst, next_scd)
  ; diagnostics = []
  ; error_occured = false
  ; ops }

let parser_advance parser =
  let tok = parser.current_tok in
  parser.current_tok <- fst parser.next_two_toks;
  let lexer, next_snd = Lexer.next parser.lexer in
  parser.next_two_toks <- (snd parser.next_two_toks, next_snd);
  parser.lexer <- lexer;
  tok

let parser_consume parser tok diagnostic_msg =
  if parser.current_tok = tok then
    Some (parser_advance parser)
  else 
    ( add_diagnostic parser diagnostic_msg parser.current_tok.span
    ; None
    )

let get_ast_unop parser =
  match parser.current_tok.kind with
  | Token.NotOp -> Some Ast.Not

  | Token.IntNegOp -> Some Ast.IntNeg
  | Token.FloatNegOp -> Some Ast.FloatNeg

  | Token.StrLenOp -> Some Ast.StrLen

  | Token.ListLenOp -> Some Ast.ListLen

  | Token.RecordLenOp -> Some Ast.RecordLen

  | Token.RefOp -> Some Ast.Ref
  | Token.DerefOp -> Some Ast.Deref

  | Token.Operator -> Some (Ast.Op (Code.read parser.code parser.current_tok.span))
  | _ -> None

let get_ast_binop parser =
  match parser.current_tok.kind with
  | Token.AndOp -> Some Ast.And
  | Token.OrOp  -> Some Ast.Or

  | Token.IntAddOp -> Some Ast.IntAdd
  | Token.IntSubOp -> Some Ast.IntSub
  | Token.IntMulOp -> Some Ast.IntMul
  | Token.IntDivOp -> Some Ast.IntDiv
  | Token.IntExpOp -> Some Ast.IntExp

  | Token.FloatAddOp -> Some Ast.FloatAdd 
  | Token.FloatSubOp -> Some Ast.FloatSub
  | Token.FloatMulOp -> Some Ast.FloatMul
  | Token.FloatDivOp -> Some Ast.FloatDiv
  | Token.FloatExpOp -> Some Ast.FloatExp

  | Token.StrAppendOp -> Some Ast.StrAppend 
  | Token.StrConcatOp -> Some Ast.StrConcat 
  | Token.StrCharAtOp -> Some Ast.StrCharAt

  | Token.ListAppendOp -> Some Ast.ListAppend
  | Token.ListIndexOp -> Some Ast.ListIndex

  | Token.RecordIndexOp -> Some Ast.RecordIndex

  | Operator -> Some (Ast.Op (Code.read parser.code parser.current_tok.span))
  | _ -> None

let rec parse_expr parser = parse_pratt parser 0

and parse_pratt parser min_bp =
  let lhs = parse_expr parser min_bp in
  parser

and parse_primary parser =
  match parser.current_tok.kind with
  | Token.Identifier -> parse_binding parser
  | Token.Integer -> parse_integer parser
  | Token.Float -> parse_integer parser
  | Token.String -> parse_integer parser
  | _ ->
    add_diagnostic parser "expected a primary expression" parser.current_tok.span;
    Ast.{ kind = Invalid; span = parser.current_tok.span }

and parse_binding parser =
  let span = (parser_advance parser).span in
  let integer = Code.read parser.code span in
  Ast.{ kind = Binding integer; span }

and parse_integer parser =
  let span = (parser_advance parser).span in
  let integer = int_of_string (Code.read parser.code span) in
  Ast.{ kind = Integer integer; span }

and parse_string parser =
  let span = (parser_advance parser).span in
  let string = Code.read parser.code span in
  Ast.{ kind = String string; span = span }

and parse_float parser =
  let span = (parser_advance parser).span in
  let integer = float_of_string (Code.read parser.code span) in
  Ast.{ kind = Float integer; span }

let parse p = []
