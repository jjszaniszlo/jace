
(* ast node *)
type 'a t = { kind: 'a; span: Span.t }

type identifier = string
type generic = string

type binop =
  (* primitive bin ops *)
  | And    (* '@and' *)
  | Or     (* '@or' *)

  (* integer primitives *)
  | IntAdd (* '@intAdd' *)
  | IntSub (* '@intSub' *)
  | IntMul (* '@intMul' *)
  | IntDiv (* '@intDiv' *)
  | IntExp (* '@intExp' *)

  (* float primitives *)
  | FloatAdd (* '@floatAdd' *)
  | FloatSub (* '@floatSub' *)
  | FloatMul (* '@floatMul' *)
  | FloatDiv (* '@floatDiv' *)
  | FloatExp (* '@floatExp' *)

  (* string primitive operators *)
  | StrAppend (* '@strAppend' *)
  | StrConcat (* '@strConcat' *)
  | StrCharAt (* '@strCharAt' *)

  (* list primitive operations *)
  | ListAppend  (* '@listAppend' *)
  | ListIndex   (* '@listIndex' *)

  | RecordIndex  (* '@recordIndex' *)

  | Op of string

type unop =
  | Not (* '@not' *)

  (* number primitives *)
  | IntNeg   (* '@intNeg' *)
  | FloatNeg (* '@floatNeg' *)

  (* string primitive operators *)
  | StrLen    (* '@strLen' *)

  (* list primitive operations *)
  | ListLen (* '@listLen' *)

  (* record primitive operations *)
  | RecordLen (* '@recordLen' *)

  (* ref and deref *)
  | Ref   (* '@ref' *)
  | Deref (* '@deref' *)

  | Op of string

(*(*  *)*)
(*type def =*)
(*  | Type of (identifier * generic list * ty t list) t*)
(*  | Class of (identifier * generic * def list) t*)
(*  | AssignTy of (identifier * ty t) t*)
(**)
(*(* type annotation / def *)*)
(*(* this is a general container used in a variety of places. *)*)
(*and ty = *)
(*  | Type of identifier*)
(*  | Generic of generic*)
(*  | Variant of identifier * ty t*)
(*  | Specialization of identifier * ty t*)
(*  | List of ty t * int option*)
(*  (* fn are exponential types, but as an annotation they are represented as a product here *)*)
(*  | Fn of ty t * ty t*)
(*  | Product of ty t * ty t*)
(*  | Record of (identifier * ty) t list*)


(* a stmt is pretty much an expr with the ZeroExpr type *)
type stmt =
  | Expr of expr

  (* only identifier expr and tuple expr can be binded to *)
  (* expr := expr *)
  | Bind of expr t * expr t

  (* only references can be reassigned *)
  (* accessing a record implicitly references and using the 'ref' keyword when binding for anything else *)
  (* expr = expr *)
  | Reassign of expr t * expr t

and expr =
  (* expr binop expr *)
  | Binary of binop * expr t * expr t
  (* unop expr *)
  | Unary of unop * expr t * expr t

  (* 'if' expr 'then' expr ('elseif' expr 'then' expr)* 'else' expr *)
  | IfThen of (expr t * expr t) list * expr t

  (* 'case' expr 'in' (expr (',' expr)* '=>' expr) *)
  | CaseIn of expr t * (expr t list * expr t) list

  (* 'let' stmt* 'in' expr *)
  | LetIn of stmt t list * expr t
  
  (* '[' expr (',' expr)* ']' *)
  | List of expr t list

  (* the structure for both pattern matching code
     and record type construction *)
  (* inherit is a way of making a record have all the fields of another record *)
  (* Env is basically just a Map implementation with some extra capabilities *)
  (* '{' ('inherit' ident ';')? (ident ('=' expr)? )+ '}' *)
  | Record of expr t Env.t * expr t

  (* expr expr* *)
  | Application of expr t * expr t list

  (* the parser doesn't construct this, but the type checker will convert
     applications to this *)
  (* identifier expr* *)
  | TypeConstructor of identifier * expr t
  
  (* params => expr *)
  | Lambda of (expr t list * expr t) list

  (* expr (';' expr)+ expr *)
  | MultiExpr of expr t list * expr t

  (* syntax sugar for multiple assignment and clarity *)
  (* '(' expr (',' expr)* ') *)
  | Tuple of expr t list

  | Binding of identifier 
  | String of string
  | Integer of string
  | Float of string

  (* {} *)
  | EmptyRecord

  (* () *)
  | EmptyExpr

  | Invalid 

type jace_module = stmt t list
