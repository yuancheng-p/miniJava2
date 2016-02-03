open AST
open Type
open TAST
open EnvType
open Helper

(* This function checks that a type exists for the given classname, and returns it. *)
let type_of_method_signature methods key =
  (* TODO raise error if not found *)
  let v = Env.find methods key
    in v.return_type


let type_value v =
  match v with
  | AST.Int i -> TInt(int_of_string i)
  | AST.Float f -> TFloat(float_of_string f)
  | AST.String s -> TString(s)
  | AST.Char c -> TChar(c)
  | AST.Boolean b -> TBoolean(b)
  | AST.Null -> TNull


let type_exprssion_desc edesc =
  match edesc with
  | Val v -> TVal(type_value v)


let rec type_statement_list env l =
  let type_stmt stmt =
    match stmt with
    | Expr e -> TExpr({t_edesc=(type_exprssion_desc e.edesc)})
  in match l with
    | [] -> []
    | t::q -> (type_stmt t)::(type_statement_list env q)


let rec type_method_list env class_curr l =
  let typed_method m =
    {
      t_mbody = (type_statement_list env m.mbody);
      t_mreturntype = m.mreturntype
    }
  in match l with
     | [] -> []
     | t::q -> (typed_method t)::(type_method_list env class_curr q)


let typing ast verbose =
  let env = GlobalEnv.build_global_env ast verbose
  in let rec type_rec_structure_tree_list tree_list =
    (* This inner function receives a non-located class_or_expr *)
    let type_structure t =
      let ref_type={tpath = (trim_option_type ast.package []); tid = t.id}
      in let cur_cls = Env.find env ref_type
      in match t.info with
      | Class c ->
          TClass({
            t_cmethods = (type_method_list env cur_cls c.cmethods)
          })
    in match tree_list with
    | [] -> []
    | t::q -> type_structure t::(type_rec_structure_tree_list q)
  in
  type_rec_structure_tree_list ast.type_list

