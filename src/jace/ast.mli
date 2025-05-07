type 'a t = { kind: 'a; span: Span.t }

type ident = string

(* Types *)
type typ =
  | TypeIdent of ident
  | TypeApp of ident t * typ t list
  | RecordType of (ident * typ t) list
  | TupleType of typ t list

(* Function Types *)
type func_type =
  | FuncType of typ t * typ t

(* Operator metadata embedded in class members *)
type op_fixity = [`Infix | `Prefix | `Postfix]

type class_member =
  | Method of ident * func_type
  | Operator of {
      symbol: string;
      fixity: op_fixity;
      precedence: int;
      signature: func_type;
    }

(* Variants for ADTs *)
type variant =
  | Variant of ident * typ t list

(* Patterns for match and parameters *)
type pattern =
  | PVar of ident
  | PTuple of pattern t list
  | PConstr of ident * pattern t list

(* Primitive Operators *)
type primitive_op =
  (* logical primitives *)
  | AndOp       (* '@and' *)
  | NotOp       (* '@not' *)
  | OrOp        (* '@or' *)

  (* integer primitives *)
  | IntAddOp    (* '@intAdd' *)
  | IntSubOp    (* '@intSub' *)
  | IntMulOp    (* '@intMul' *)
  | IntDivOp    (* '@intDiv' *)
  | IntExpOp    (* '@intExp' *)
  | IntNegOp    (* '@intNeg' *)

  (* float primitives *)
  | FloatAddOp  (* '@floatAdd' *)
  | FloatSubOp  (* '@floatSub' *)
  | FloatMulOp  (* '@floatMul' *)
  | FloatDivOp  (* '@floatDiv' *)
  | FloatExpOp  (* '@floatExp' *)
  | FloatNegOp  (* '@floatNeg' *)

  (* string primitives *)
  | StrLenOp      (* '@strLen' *)
  | StrAppendOp   (* '@strAppend' *)
  | StrConcatOp   (* '@strConcat' *)
  | StrCharAtOp   (* '@strCharAt' *)

  (* list primitives *)
  | ListLenOp     (* '@listLen' *)
  | ListAppendOp  (* '@listAppend' *)
  | ListIndexOp   (* '@listIndex' *)

  (* record primitives *)
  | RecordIndexOp (* '@recordIndex' *)
  | RecordLenOp   (* '@recordLen' *)

  (* references *)
  | RefOp         (* '@ref' *)
  | DerefOp       (* '@deref' *)

(* Expressions *)
type expr =
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Var of ident
  | Apply of expr t * expr t list
  | Lambda of (ident * typ t option) list * typ t option * expr t
  | LetIn of ident * expr t * expr t
  | Match of expr t * (pattern t * expr t) list
  | If of expr t * expr t * expr t
  | Return of expr t
  | Record of record_expr
  | PrimitiveOp of primitive_op
  | UnitLit

and record_expr =
  | RecordOnly of (ident * expr t) list

(* Top-Level Declarations *)
type decl =
  | LetVal of ident * expr t
  | LetFun of ident * (ident * typ t option) list * typ t option * expr t
  | TypeDecl of ident * variant list
  | ClassDecl of ident * class_member list
  | InstanceDecl of ident * typ t * (ident * expr t) list

(* Program *)
type program =
  | Program of decl t list
