type t

val init : Code.t -> t

val parse_program : t -> Ast.program Ast.t
val parse_decl : t -> Ast.decl Ast.t
val parse_let_decl : t -> Ast.decl Ast.t
val parse_lambda : t -> Ast.expr Ast.t
val parse_expr : t -> Ast.expr Ast.t
val parse_type : t -> Ast.typ Ast.t
val parse_param_list : t -> (Ast.ident * Ast.typ Ast.t option) list
val parse_param : t -> Ast.ident * Ast.typ Ast.t option
val parse_return_type_opt : t -> Ast.typ Ast.t option
