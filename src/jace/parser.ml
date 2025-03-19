open Base

type op_precedence =
  { prefix: int Env.t
  ; infix: (int * int) Env.t
  ; postfix: int Env.t
  }

type t = 
  { lexer : Lexer.t
  ; code : Code.t
  ; current_tok : Token.t
  ; next_two_toks : Token.t * Token.t
  ; diagnostics : Diagnostic.t list
  ; error_occured : bool 
  ; ops : op_precedence 
  }

let ( let* ) res f = Base.Result.bind res ~f

let add_diagnostic parser msg span =
  if parser.error_occured then
    parser
  else 
    { parser with error_occured = true
    ; diagnostics = Diagnostic.{msg; span} :: parser.diagnostics
    }

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
  ; ops
  }

(* get current tok and advance parser *)
let parser_advance parser =
  let tok = parser.current_tok in
  let lexer, next_snd = Lexer.next parser.lexer in
  let parser =
    { parser with current_tok = fst parser.next_two_toks
    ; next_two_toks = (snd parser.next_two_toks, next_snd)
    ; lexer
    }
  in
  parser, tok

let parser_consume parser tok diagnostic_msg =
  let open Base.Poly in
  if parser.current_tok.kind = tok then
    let parser, tok = parser_advance parser in
    parser, Some tok
  else 
    let parser = add_diagnostic parser diagnostic_msg parser.current_tok.span in
    parser, None

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
  parser

and parse_primary parser =
  match parser.current_tok.kind with
  | _ ->
    let parser = add_diagnostic parser "expected a primary expression" parser.current_tok.span in
    parser, Ast.{ kind = Invalid; span = parser.current_tok.span }

(* these are helper functions which consume immediately, because it is known what their values is via lookahead in other parts of the parser. *)
and consume_binding parser =
  let parser, tok = parser_advance parser in
  let span = tok.span in
  let integer = Code.read parser.code span in
  Ok (parser, Ast.{ kind = Binding integer; span })

and consume_integer parser =
  let parser, tok = parser_advance parser in
  let span = tok.span in
  let integer = Code.read parser.code span in
  Ok (parser, Ast.{ kind = Integer integer; span })

and consume_string parser =
  let parser, tok = parser_advance parser in
  let span = tok.span in
  let string = Code.read parser.code span in
  Ok (parser, Ast.{ kind = String string; span = span })

and consume_float parser =
  let parser, tok = parser_advance parser in
  let span = tok.span in
  let float = Code.read parser.code span in
  Ok (parser, Ast.{ kind = Float float; span })

let parse p = []
