open AST
open Type
open TAST
open Helper

let type_value v =
  match v with
  | AST.Int i -> TInt(int_of_string i)
  | AST.Float f -> TFloat(float_of_string f)
  | AST.String s -> TString(s)
  | AST.Char c -> TChar(c)
  | AST.Boolean b -> TBoolean(b)
  | AST.Null -> TNull


let type_expression_desc env edesc =
  match edesc with
  | Val v -> TVal(type_value v)
  (* TODO: check and type all the expressions here *)


let type_expression env e =
  {
    t_edesc = type_expression_desc env e.edesc;
  }


let rec type_var_decl_list env vd_list =
  let type_var_decl env vd =
    (* TODO: add vars into env *)
    match vd with
    | (t, s, Some e) -> (t, s, Some (type_expression env e))
    | (t, s, None) -> (t, s, None)
  in match vd_list with
  | [] -> []
  | h::others -> type_var_decl env h::(type_var_decl_list env others)


let rec type_statement_list env l =
  let type_statment stmt =
    match stmt with
    | VarDecl vd_list -> TVarDecl(type_var_decl_list env vd_list)
    | Expr e -> TExpr(type_expression env e)
    (*TODO: check and type all the statments here *)

  in match l with
    | [] -> []
    | t::q -> (type_statment t)::(type_statement_list env q)


let rec type_method_list env class_curr l =
  let typed_method m =
    {
      t_mbody = (type_statement_list env m.mbody);
      t_mreturntype = m.mreturntype
      (* TODO: t_mname, t_margstype, t_mthrows *)
    }
  in match l with
     | [] -> []
     | t::q -> (typed_method t)::(type_method_list env class_curr q)


let typing ast verbose =
  let env = GlobalEnv.build_global_env ast verbose

  (* for each AST, turn it into TAST *)
  in let rec type_type_list type_list =

    let type_asttype asttype =

      let type_type_info t =
        let ref_type = {tpath = []; tid = t.id}
        in let cur_cls = Env.find env ref_type
        in match t.info with
        | Class c ->
            TClass({
              t_cmethods = (type_method_list env cur_cls c.cmethods)
            })

      in {
        t_modifiers = asttype.modifiers;
        t_id = asttype.id;
        t_info = type_type_info asttype;
      }

    in match type_list with
    | [] -> []
    | h::others -> type_asttype h::(type_type_list others)

  in {
    t_package = ast.package;
    t_type_list = type_type_list ast.type_list
  }
