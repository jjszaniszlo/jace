type kind =
  | Invalid
  | Eof

  (* literals *)
  | Identifier
  (* '<identifier> i.e 'a *)
  | Generic
  | Integer
  | Float
  | String
  | InvalidString

  (* operators are isomorphic to identifiers which bind to functions *)
  | Operator

  (* these are binding reserved operators *)
  | Equal       (* = *)
  | ColonEqual  (* := *)
  | ColonColon  (* :: *)

  (* primitive operators *)

  (* logical primitives *)
  | AndOp    (* '@and' *)
  | NotOp    (* '@not' *)
  | OrOp     (* '@or' *)

  (* integer primitives *)
  | IntAddOp (* '@intAdd' *)
  | IntSubOp (* '@intSub' *)
  | IntMulOp (* '@intMul' *)
  | IntDivOp (* '@intDiv' *)
  | IntExpOp (* '@intExp' *)
  | IntNegOp (* '@intNeg' *)

  (* float primitives *)
  | FloatAddOp (* '@floatAdd' *)
  | FloatSubOp (* '@floatSub' *)
  | FloatMulOp (* '@floatMul' *)
  | FloatDivOp (* '@floatDiv' *)
  | FloatExpOp (* '@floatExp' *)
  | FloatNegOp (* '@floatNeg' *)

  (* string primitive operators *)
  | StrLenOp    (* '@strLen' *)
  | StrAppendOp (* '@strAppend' *)
  | StrConcatOp (* '@strConcat' *)
  | StrCharAtOp   (* '@strCharAt' *)

  (* list primitive operations *)
  | ListLenOp     (* '@listLen' *)
  | ListAppendOp  (* '@listAppend' *)
  | ListIndexOp   (* '@listIndex' *)

  (* record operators *)
  | RecordIndexOp    (* '@recordIndex' *)
  | RecordLenOp      (* '@recordLen' *)

  (* ref and deref *)
  | RefOp         (* '@ref' *)
  | DerefOp       (* '@deref' *)

  | InvalidPrimitiveOp
 
  (* delimiters *)
  | SemiColon
  | Comma
  | BackSlash
  | FatArrow
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket

  (* keywords *)
  | TypeKeyword
  | ClassKeyword
  | InstanceKeyword
  | DefKeyword
  | WhereKeyword
  | LetKeyword
  | InKeyword
  | CaseKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | ElseIfKeyword
  | RefKeyword
  | InheritKeyword

type t =
  { kind: kind
  ; span: Span.t
  }
