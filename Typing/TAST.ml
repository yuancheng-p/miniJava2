type t_argument = {
    t_final : bool;
    t_vararg : bool;
    t_ptype : Type.t;
    t_pident : string;
  }

type t_value =
  | TString of string
  | TInt of int
  | TFloat of float
  | TChar of char option
  | TNull
  | TBoolean of bool

type t_expression_desc =
  | TVal of t_value
  | TName of string * Type.t
  | TAssignExp of t_expression * AST.assign_op * t_expression * Type.t
  | TNew of string option * string list * t_expression list * Type.t
  | TNewArray of Type.t * (t_expression option) list * t_expression option
  | TCall of t_expression option * string * t_expression list
  | TAttr of t_expression * string
  | TIf of t_expression * t_expression * t_expression
  | TArrayInit of t_expression list
  | TArray of t_expression * (t_expression option) list
  | TPost of t_expression * AST.postfix_op
  | TPre of AST.prefix_op * t_expression
  | TOp of t_expression * AST.infix_op * t_expression * Type.t
  | TCondOp of t_expression * t_expression * t_expression
  | TCast of Type.t * t_expression
  | TType of Type.t
  | TClassOf of Type.t
  | TInstanceof of t_expression * Type.t
  | TVoidClass
  | TQN of string list

and t_expression = {
    t_edesc : t_expression_desc;
    (*TODO should we add a type?: t_type : Type.t; *)
(*   t_eloc : Location.t;
     mutable t_etype : Type.t option;*)
}

type t_astattribute = {
      mutable amodifiers : AST.modifier list;
      t_aname : string;
      t_atype : Type.t;
      t_adefault : t_expression option;
      (*      aloc : Location.t;*)
    }

type t_statement =
  | TExpr of t_expression
  | TVarDecl of (Type.t * string * t_expression option) list
  | TBlock of t_statement list
  | TNop
  | TWhile of t_expression * t_statement
  | TFor of (Type.t option * string * t_expression option) list * t_expression option * t_expression list * t_statement
  | TIf of t_expression * t_statement * t_statement option
  | TReturn of t_expression option
  | TThrow of t_expression
  | TTry of t_statement list * (t_argument * t_statement list) list * t_statement list

type t_astmethod = {
    t_margstype : t_argument list;
    t_mbody : t_statement list;
    t_mreturntype : Type.t; (* TODO: what should we do? *)
(* TODO  mutable t_mmodifiers : AST.modifier list;
    t_mname : string;
    t_mthrows : Type.ref_type list; *)
}

type t_astconst = {
    mutable t_cmodifiers : AST.modifier list;
    t_cname : string;
    t_cargstype : t_argument list;
    t_cthrows : Type.ref_type list;
    t_cbody : t_statement list;
    (*      t_mloc : Location.t;*)
  }

type t_astclass = {
    t_cmethods : t_astmethod list;
(* TODO  t_cparent : Type.ref_type;
    t_cattributes : t_astattribute list;
    t_cinits : t_initial list;
    t_cconsts : t_astconst list;
    t_ctypes : t_asttype list;
    t_cloc : Location.t; *)
  }

type t_type_info =
  | TClass of t_astclass
  | TInter

type t_initial = {
    t_static : bool ;
    t_block : t_statement list
  }

and t_asttype = {
    mutable t_modifiers : AST.modifier list;
    t_id : string;
    t_info : t_type_info;
  }

type t_t = {
    t_package : AST.qualified_name option;
    t_type_list : t_asttype list;
  }
