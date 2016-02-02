open AST
open Type
open TAST
open EnvType
open Helper

(* This function checks that a type exists for the given classname, and returns it. *)
(* The classname is a non-located Structure.classname. *)
let type_of_method_signature methods key =
  (* TODO raise error if not found *)
  let v = Env.find methods key
    in v.return_type

  (*
let rec type_params_list classesEnv params = match params with
	| [] -> []
	| t::q -> (match (Located.elem_of t) with
			| Param(c, s) -> Located.mk_elem (TypedParam(c, s, type_of_classname classesEnv (Located.elem_of c) (Located.loc_of c)))
					(Located.loc_of t)::(type_params_list classesEnv q)
		)
  *)

let rec type_method_list classesEnv class_curr l =
  let type_method m is_static =
    let mtype = Classname(Type.stringOf m.mreturntype)
    and mname = m.mname
    and margstype = m.margstype
    and mbody = m.mbody
    and mthrows = m.mthrows
    (* in let nparams = type_params_list classesEnv params *)
    in let t_args = mk_t_args m.margstype [] in
    let key = {name=mname; args=t_args} in
    let return_type = type_of_method_signature class_curr.methods key in
    (* TODO: check if return_type is the same as the return statement type *)
    (*in let ne = type_expr (Some currentClassEnv.name)
     (not is_static) classesEnv ((parse_attributes currentClassEnv.attributes)@params_vartypes) mbody
     *)
    if (is_static) then
      TypedStaticMethod(mtype, mname, [], return_type)
    else
      TypedMethod(mtype, mname, [], return_type)

  in let typed_method m =
    let p a =
      match a with (* TODO inline me *)
      | Static ->  true
      | _ -> false
    in match List.exists p m.mmodifiers with
       | true -> type_method m true
       | false -> type_method m false
  in match l with
     | [] -> []
     | t::q -> (typed_method t)::(type_method_list classesEnv class_curr q)

(*margstype : argument list;
    mthrows : Type.ref_type list;
    mbody : statement list;*)



let rec type_attr_list classesEnv class_curr l =
  let typed_attr a =
    let atype = Classname(Type.stringOf a.atype)
    and aname = a.aname
    and amodifiers = a.amodifiers
    in match a.adefault with
      | None -> TypedAttr(atype, aname)
      (*TODO | None -> TypedAttr(aname,atype,type_of_classname classesEnv r_type aname)*)
      (*TODO | Some e -> type_attr_with_value atype aname e*)
  in match l with
  | [] -> []
  | t::q -> (typed_attr t)::(type_attr_list classesEnv class_curr q)



let typing ast verbose =
  let env = GlobalEnv.build_global_env ast verbose
  in let rec type_rec_structure_tree sub_tree =
    (* This inner function receives a non-located class_or_expr *)
    let type_structure t=
      let ref_type={tpath = (trim_option_type ast.package []); tid = t.id}
      in let class_curr = Env.find env ref_type
      in match t.info with
      | Class c -> TypedClassdef(
          type_attr_list env class_curr c.cattributes,
          type_method_list env class_curr c.cmethods)
    in match sub_tree with
    | [] -> []
    | t::q -> type_structure t::(type_rec_structure_tree q)
  in
  (*type_rec_structure_tree ast.type_list*)
  ()

