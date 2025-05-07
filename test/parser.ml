open OUnit2

let parse source =
  let open Jace in
  let code = Code.init source in
  let parser = Parser.init code in
  Parser.parse_program parser

(*let assert_program ?msg expected actual =*)
(*  assert_equal*)
(*    ~cmp:(fun a b -> a.kind = b.kind)*)
(*    ~printer:(fun prog -> "[program parsed]")*)
(*    ?msg expected actual*)

let test_let_val _ =
  let prog = parse "let x = 42" in
  match prog.kind with
  | Program [ { kind = LetVal ("x", { kind = IntLit 42; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected let value declaration"

let test_let_fun_simple _ =
  let prog = parse "let add(a, b) = a" in
  match prog.kind with
  | Program [ { kind = LetFun ("add", [("a", None); ("b", None)], None, { kind = Var "a"; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected let function declaration"

let test_let_fun_with_types _ =
  let prog = parse "let add(a: Int, b: Int): Int = a" in
  match prog.kind with
  | Program [ { kind = LetFun ("add", [("a", Some _); ("b", Some _)], Some _, { kind = Var "a"; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected typed let function declaration"

let test_lambda _ =
  let prog = parse "let f = fn (x, y) => x" in
  match prog.kind with
  | Program [ { kind = LetVal ("f", { kind = Lambda ([("x", None); ("y", None)], None, { kind = Var "x"; _ }); _ }); _ } ] -> ()
  | _ -> assert_failure "Expected lambda expression in let binding"

let test_unit _ =
  let prog = parse "let unit = ()" in
  match prog.kind with
  | Program [ { kind = LetVal ("unit", { kind = UnitLit; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected unit literal"

let test_int _ =
  let prog = parse "let a = 5" in
  match prog.kind with
  | Program [ { kind = LetVal ("a", { kind = IntLit 5; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected int literal"

let test_float _ =
  let prog = parse "let b = 3.14" in
  match prog.kind with
  | Program [ { kind = LetVal ("b", { kind = FloatLit f; _ }); _ } ] when abs_float (f -. 3.14) < 0.001 -> ()
  | _ -> assert_failure "Expected float literal"

let test_string _ =
  let prog = parse "let s = \"hello\"" in
  match prog.kind with
  | Program [ { kind = LetVal ("s", { kind = StringLit "hello"; _ }); _ } ] -> ()
  | _ -> assert_failure "Expected string literal"

let suite =
  "Parser Test Suite" >::: [
    "let value" >:: test_let_val;
    "let function (simple)" >:: test_let_fun_simple;
    "let function (typed)" >:: test_let_fun_with_types;
    "lambda" >:: test_lambda;
    "unit" >:: test_unit;
    "int literal" >:: test_int;
    "float literal" >:: test_float;
    "string literal" >:: test_string;
  ]

let () =
  run_test_tt_main suite
