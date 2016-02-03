type t_value =
  | TString of string
  | TInt of int
  | TFloat of float
  | TChar of char option
  | TNull
  | TBoolean of bool

type t_expression_desc =
  | TVal of t_value

type t_expression = {
      t_edesc : t_expression_desc;
}

type t_statement =
  | TExpr of t_expression

type t_astmethod = {

    t_mbody : t_statement list;
    t_mreturntype : Type.t;

  (*
    mutable t_mmodifiers : modifier list;
    t_mname : string;
    t_margstype : argument list;
    t_mthrows : Type.ref_type list;
    *)
}

type t_astclass = {
    t_cmethods : t_astmethod list;

    (*
    t_cparent : Type.ref_type;
    t_cattributes : astattribute list;
    t_cinits : initial list;
    t_cconsts : astconst list;
    t_ctypes : asttype list;
    t_cloc : Location.t; *)
  }


type typed_class_or_expr =
  | TClass of t_astclass

