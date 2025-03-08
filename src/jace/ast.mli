
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
  | StrCharAt (* '@strCharAt' *)

  (* list primitive operations *)
  | ListLen (* '@listLen' *)

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
  (* ident := expr *)
  | Bind of identifier * expr t
  (* expr = expr *)
  | Rebind of expr t * expr t
  (* can contain a single function body
     or a polymorphic function body with case *)
  | Function of identifier * (expr t list * expr t list) list

and expr =
  (* infix *)
  | Binary of binop * expr t * expr t
  (* prefix or postfix *)
  | Unary of unop * expr t * expr t

  (* if then elseif else, etc *)
  | IfThen of (expr t * expr t) list * expr t
  (* case expr branches *)
  | CaseIn of expr t * (expr t list * expr t list) list

  | LetIn of stmt t list

  | Call of expr t * expr t list

  (* TODO constrain record keys to some E *)
  | RecordEntry of expr t * expr t
  | List of expr t list

  (* variant constructor,
     looks similar to a function call, so it will likely be handled by the
     type checker *)
  (* TODO constrain identifier to an environment E *)
  | Variant of identifier * expr t list

  | String of string
  | Integer of int
  | Float of float

  | Fn of identifier list * expr t list

  | Identifier of identifier

  | EmptyRecord
  | EmptyExpr
  | Invalid 

type jace_module = stmt t list
