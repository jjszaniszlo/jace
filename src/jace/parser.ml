(*exception SyntaxError of Diagnostic.t list*)

type t = {
  source : Code.t;
  lexer : Lexer.t;
  mutable token : Token.t;
  mutable next_tokens : Token.t * Token.t;
  mutable diagnostics : Diagnostic.t list;
  mutable panic : bool;
}

let init code =
  let lexer = Lexer.init code in
  let lexer, token = Lexer.next lexer in
  let lexer, next1 = Lexer.next lexer in
  let lexer, next2 = Lexer.next lexer in
  {
    source = code;
    lexer;
    token;
    next_tokens = (next1, next2);
    diagnostics = [];
    panic = false;
  }

let advance p =
  let tok = p.token in
  let next1, next2 = p.next_tokens in
  p.token <- next1;
  p.next_tokens <- (next2, snd (Lexer.next p.lexer));
  tok

let emit_diagnostic p msg span =
  if not p.panic then (
    p.panic <- true;
    p.diagnostics <- Diagnostic.{ msg; span } :: p.diagnostics)

let expect p kind message =
  if p.token.kind = kind then Some (advance p)
  else (
    emit_diagnostic p message p.token.span;
    None)

let parse_name message p =
  match expect p Identifier message with
  | Some tok -> Code.read p.source tok.span
  | None -> "_"

let rec parse_program p =
  let open Ast in
  let rec loop decls =
    match p.token.kind with
    | Eof -> { kind = Program decls; span = p.token.span }
    | _ ->
      let decl = parse_decl p in
      loop (decls @ [decl])
  in
  loop []

and parse_type_decl p =
  let open Ast in
  emit_diagnostic p "type declarations not yet implemented" p.token.span;
  { kind = TypeDecl ("error", []); span = p.token.span }

and parse_class_decl p =
  let open Ast in
  emit_diagnostic p "class declarations not yet implemented" p.token.span;
  { kind = ClassDecl ("error", []); span = p.token.span }

and parse_instance_decl p =
  let open Ast in
  emit_diagnostic p "instance declarations not yet implemented" p.token.span;
  { kind = InstanceDecl ("error", { kind = TypeIdent "error"; span = p.token.span }, []); span = p.token.span }

and parse_decl (p: t): Ast.decl Ast.t =
  let open Ast in
  match p.token.kind with
  | TypeKeyword -> parse_type_decl p
  | ClassKeyword -> parse_class_decl p
  | InstanceKeyword -> parse_instance_decl p
  | LetKeyword -> parse_let_decl p
  | _ ->
    let _ = advance p in
    emit_diagnostic p "expected top-level declaration" p.token.span;
    { kind = LetVal ("_", { kind = Var "_"; span = p.token.span }); span = p.token.span }

and parse_let_decl (p: t): Ast.decl Ast.t =
  let open Ast in
  let open Span in
  let start = (advance p).span in
  let name = parse_name "expected name after let" p in
  match p.token.kind with
  | LeftParen ->
    let args = parse_param_list p in
    let return_type = parse_return_type_opt p in
    ignore (expect p Equal "expected '='");
    let body = parse_expr p in
    { kind = LetFun (name, args, return_type, body); span = merge start body.span }
  | Equal ->
    ignore (advance p);
    let body = parse_expr p in
    { kind = LetVal (name, body); span = merge start body.span }
  | _ ->
    emit_diagnostic p "expected '(' or '=' after identifier" p.token.span;
    { kind = LetVal (name, { kind = Var "error"; span = p.token.span }); span = p.token.span }

and parse_param_list p =
  ignore (expect p LeftParen "expected '(' for parameters");
  let rec loop acc =
    match p.token.kind with
    | RightParen -> ignore (advance p); List.rev acc
    | _ ->
      let param = parse_param p in
      let acc = param :: acc in
      if p.token.kind = Comma then ignore (advance p);
      loop acc
  in
  loop []

and parse_param p =
  let name = parse_name "expected parameter name" p in
  let typ =
    match p.token.kind with
    | Colon -> ignore (advance p); Some (parse_type p)
    | _ -> None
  in
  (name, typ)

and parse_return_type_opt p =
  match p.token.kind with
  | Colon -> ignore (advance p); Some (parse_type p)
  | _ -> None

and parse_type p =
  match p.token.kind with
  | Identifier ->
    let id = parse_name "expected type identifier" p in
    { kind = TypeIdent id; span = p.token.span }
  | _ ->
    emit_diagnostic p "invalid type" p.token.span;
    { kind = TypeIdent "error"; span = p.token.span }

and parse_expr p =
  match p.token.kind with
  | FnKeyword -> parse_lambda p
  | _ -> parse_var_expr p

and parse_lambda p =
  let open Span in
  let start = (advance p).span in
  let params = parse_param_list p in
  let return_type = parse_return_type_opt p in
  ignore (expect p FatArrow "expected '=>' after lambda");
  let body = parse_expr p in
  { kind = Lambda (params, return_type, body); span = merge start body.span }

and parse_var_expr p =
  let open Ast in
  let open Span in
  match p.token.kind with
  | Identifier ->
    let name = parse_name "expected variable name" p in
    { kind = Var name; span = p.token.span }
  | LeftParen ->
    let span = (advance p).span in
    if p.token.kind = RightParen then (
      ignore (advance p);
      { kind = UnitLit; span = merge span p.token.span }
    ) else
      let e = parse_expr p in
      ignore (expect p RightParen "expected ')' after expression");
      e
  | Integer ->
    let tok = advance p in
    let lit = int_of_string (Code.read p.source tok.span) in
    { kind = IntLit lit; span = tok.span }
  | Float ->
    let tok = advance p in
    let lit = float_of_string (Code.read p.source tok.span) in
    { kind = FloatLit lit; span = tok.span }
  | String ->
    let tok = advance p in
    let s = Code.read p.source tok.span in
    let len = String.length s in
    let unquoted = String.sub s 1 (len - 2) in
    { kind = StringLit unquoted; span = tok.span }
  | _ ->
    emit_diagnostic p "unexpected token in expression" p.token.span;
    { kind = Var "error"; span = p.token.span }
