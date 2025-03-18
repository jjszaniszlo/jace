open OUnit2

let test string tokens =
  let open Jace in
  let code = Code.init string in
  let lexer = Lexer.init code in
  let rec test' lexer tokens =
    match tokens with
      | (kind, str) :: tail ->
        let lexer, token = Lexer.next lexer in
        assert_equal kind token.kind;
        assert_equal str (Code.read code token.span);
        test' lexer tail
      | _ -> ()
  in
  test' lexer tokens
;;

let test_keywords _ =
  let open Jace.Token in
  test "type class instance def where let in case if then else elseif ref inherit"
    [ (TypeKeyword, "type")
    ; (ClassKeyword, "class")
    ; (InstanceKeyword, "instance")
    ; (DefKeyword, "def")
    ; (WhereKeyword, "where")
    ; (LetKeyword, "let")
    ; (InKeyword, "in")
    ; (CaseKeyword, "case")
    ; (IfKeyword, "if")
    ; (ThenKeyword, "then")
    ; (ElseKeyword, "else")
    ; (ElseIfKeyword, "elseif")
    ; (RefKeyword, "ref")
    ; (InheritKeyword, "inherit")
    ; (Eof, "")
    ]
;;

let test_identifiers _ =
  let open Jace.Token in
  test "foo _bar baz123 foobarbaz_123 __wow"
    [ (Identifier, "foo")
    ; (Identifier, "_bar")
    ; (Identifier, "baz123")
    ; (Identifier, "foobarbaz_123")
    ; (Identifier, "__wow")
    ]
;;

(* TODO handle invalid numbers and test them *)
let test_numbers _ =
  let open Jace.Token in
  test "123 1.23 0.23 10000 0.00004 0"
    [ (Integer, "123")
    ; (Float, "1.23")
    ; (Float, "0.23")
    ; (Integer, "10000")
    ; (Float, "0.00004")
    ; (Integer, "0")
    ; (Eof, "")
    ]
;;

let test_strings _ =
  let open Jace.Token in
  test {|"Hello World!" "Escaped: \"Foo\"" "Some more escapes:\t\n\r"|}
    [ (String, {|"Hello World!"|})
    ; (String, {|"Escaped: \"Foo\""|})
    ; (String, {|"Some more escapes:\t\n\r"|})
    ; (Eof, "")
    ]
;;

let test_invalid_strings _ =
  let open Jace.Token in
  (* bad strings still match, but are still considered invalid. *)
  test {|"Bad string then next line:
         "Bad string then eof
         "Invalid escape sequence: \a <- this guy"|}
    [ (InvalidString, {|"Bad string then next line:|})
    ; (InvalidString, {|"Bad string then eof|})
    ; (InvalidString, {|"Invalid escape sequence: \a <- this guy"|})
    ; (Eof, "")
    ]
;;

let test_delimeters _ =
  let open Jace.Token in
  test "; , \\ => ( ) { } [ ]"
    [ (SemiColon, ";")
    ; (Comma, ",")
    ; (BackSlash, "\\")
    ; (FatArrow, "=>")
    ; (LeftParen, "(")
    ; (RightParen, ")")
    ; (LeftBrace, "{")
    ; (RightBrace, "}")
    ; (LeftBracket, "[")
    ; (RightBracket, "]")
    ; (Eof, "")
    ]
;;

let test_delimeters_1 _ =
  let open Jace.Token in
  test "(())=>,\\;[]{}(=>)"
    [ (LeftParen, "(")
    ; (LeftParen, "(")
    ; (RightParen, ")")
    ; (RightParen, ")")
    ; (FatArrow, "=>")
    ; (Comma, ",")
    ; (BackSlash, "\\")
    ; (SemiColon, ";")
    ; (LeftBracket, "[")
    ; (RightBracket, "]")
    ; (LeftBrace, "{")
    ; (RightBrace, "}")
    ; (LeftParen, "(")
    ; (FatArrow, "=>")
    ; (RightParen, ")")
    ; (Eof, "")
    ]
;;

let test_reserved_operators _ =
  let open Jace.Token in
  test "= := ::"
    [ (Equal, "=")
    ; (ColonEqual, ":=")
    ; (ColonColon, "::")
    ; (Eof, "")
    ]
;;

let test_default_implemented_operators _ =
  let open Jace.Token in
  test "+ - * / : > >= < <= ^ == != . .."
    [ (Operator, "+")
    ; (Operator, "-")
    ; (Operator, "*")
    ; (Operator, "/")
    ; (Operator, ":")
    ; (Operator, ">")
    ; (Operator, ">=")
    ; (Operator, "<")
    ; (Operator, "<=")
    ; (Operator, "^")
    ; (Operator, "==")
    ; (Operator, "!=")
    ; (Operator, ".")
    ; (Operator, "..")
    ; (Eof, "")
    ]
;;

let test_primitive_operators _ =
  let open Jace.Token in
  test {| @and @not @or @intAdd @intSub @intMul @intDiv
          @intExp @floatAdd @floatSub @floatMul @floatDiv
          @floatExp @strLen @strAppend @strConcat @strCharAt
          @listLen @listAppend @listIndex @recordIndex @recordLen 
          @ref @deref|}
    [ (AndOp, "@and")    
    ; (NotOp, "@not")    
    ; (OrOp, "@or")      
    ; (IntAddOp, "@intAdd") 
    ; (IntSubOp, "@intSub") 
    ; (IntMulOp, "@intMul") 
    ; (IntDivOp, "@intDiv") 
    ; (IntExpOp, "@intExp") 
    ; (FloatAddOp, "@floatAdd") 
    ; (FloatSubOp, "@floatSub") 
    ; (FloatMulOp, "@floatMul") 
    ; (FloatDivOp, "@floatDiv") 
    ; (FloatExpOp, "@floatExp") 
    ; (StrLenOp, "@strLen")       
    ; (StrAppendOp, "@strAppend") 
    ; (StrConcatOp, "@strConcat") 
    ; (StrCharAtOp, "@strCharAt")   
    ; (ListLenOp, "@listLen")        
    ; (ListAppendOp, "@listAppend")  
    ; (ListIndexOp, "@listIndex")    
    ; (RecordIndexOp, "@recordIndex")
    ; (RecordLenOp, "@recordLen")
    ; (RefOp, "@ref")
    ; (DerefOp, "@deref")
    ; (Eof, "")
    ]
;;

let test_invalid_primitive_operators _ =
  let open Jace.Token in
  test "@foo @bar @baz"
    [ (InvalidPrimitiveOp, "@foo")
    ; (InvalidPrimitiveOp, "@bar")
    ; (InvalidPrimitiveOp, "@baz")
    ; (Eof, "")
    ]
;;

let tests = "Lexer tests"
  >:::
    [ "keywords" >:: test_keywords 
    ; "numbers" >:: test_numbers
    ; "delimeters" >:: test_delimeters 
    ; "delimeters_1" >:: test_delimeters_1 
    ; "reserved_operators" >:: test_reserved_operators
    ; "default_operators" >:: test_default_implemented_operators
    ; "primitive_operators" >:: test_primitive_operators
    ; "invalid_primitive_operators" >:: test_invalid_primitive_operators
    ; "identifiers" >:: test_identifiers
    ; "strings" >:: test_strings
    ; "invalid_strings" >:: test_invalid_strings
    ]
;;

let () = run_test_tt_main tests
