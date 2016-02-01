open AST
open Type
open TAST
open Structure

exception Class_Redifinition of string;;
exception Method_Signiture of string;;

type class_env = {
  parent: ref_type;
  methods: (t_method_signiture, method_env) Env.t;
  attributes: string list; (* TODO replace me by attributes list *)
}
and method_env = {
  mutable modifiers: modifier list;
  return_type: t;
  (* TODO throws *)
}
and  t_method_signiture = {
  name: string;
  args: t_arg list;
}
and t_arg = {
  tvararg: bool;
  tptype: t;
}


let trim_option_type pkg default =
  match pkg with
  | Some p -> p
  | None -> default

(* TODO add package *)
(**
 * check each class_key(package,classname) if it's redifined, raise error Class_Redifinition
 * and put the (tpath,class_id) in to the global_env. But here, we don't put methods and atrrs.
 **)
let build_classes_names classes_env ast =
  (* check class redefinition function*)
  let check_class_redifinition ref_type classes_env =
    let clfind = Env.mem classes_env ref_type in
      match clfind with
      | true -> raise(Class_Redifinition(ref_type.tid))
      | false -> () in
  let rec parse_asts ast_list =
    match ast_list with
    | [] -> classes_env
    | h::others -> (match h.info with
        | Class c ->
          check_class_redifinition {tpath = (trim_option_type ast.package []); tid = h.id} classes_env;
          (Env.add classes_env
              {tpath = (trim_option_type ast.package []); tid = h.id}
              {parent = c.cparent; methods = Env.initial(); attributes = []});
          parse_asts others
        | Inter -> classes_env
    )
  in parse_asts ast.type_list

let rec mk_t_args arguments l =
  match arguments with
  | [] -> l
  | h::others ->
      mk_t_args others ({tvararg=h.vararg;tptype=h.ptype}::l)

(**
 *  for each class, add its methods into its
 *  methods table (global_env -> class_env -> methods)
 **)
let build_methods global_env ast =
  let check_method_signiture_redefined methods method_signiture = 
    if Env.mem methods method_signiture then raise(Method_Signiture("method redef : "^method_signiture.name))
  in
  let rec add_methods global_env r_type cmethods =
    match cmethods with
    | [] -> global_env
    | h::others ->
        let cls_env = Env.find global_env r_type
        in let t_args = mk_t_args h.margstype [] (*TODO inline me*)
        in check_method_signiture_redefined cls_env.methods {name=h.mname; args=t_args};
        print_endline ("check add and methods:"^r_type.tid^":"^h.mname);
        Env.add cls_env.methods {name=h.mname; args=t_args} {modifiers=[];return_type=h.mreturntype};
        add_methods global_env r_type others
  (* iterate the ast list *)
  in let rec iter_asts ast_list =
    match ast_list with
    | [] -> global_env
    | h::others -> (match h.info with
        | Class c ->
            add_methods
                global_env
                {tpath=(trim_option_type ast.package []); tid=h.id} c.cmethods;
            iter_asts others
        | Inter -> global_env
    )
  in iter_asts ast.type_list


let str_of_ref_type ref_type = ref_type.tid

let print_method_signiture method_signiture =
   print_string method_signiture.name;
   (List.iter ( fun (targ) -> print_string (" paramlist: "^Type.stringOf targ.tptype^" ") ;() ) method_signiture.args)

let print_methods methods =
  Env.iter (fun(m_signiture,menv)->print_string "method : " ; print_method_signiture m_signiture;print_endline ""; ()) methods

let str_of_method_env method_env = "method env"  (* will be delete *)

let str_of_methods methods =  "TODO:print methods table" (* replace by print_method , will be delete *)

let str_of_class_env class_env =
  "class_env(parent="
      ^(String.concat "." class_env.parent.tpath)^": "^class_env.parent.tid^")\n"
      (* ^";methods="^(str_of_methods class_env.methods) *)

let print_classes_env classes_env =
  Env.iter (
    fun (r_type, c_env) ->
      print_string
      (str_of_ref_type r_type ^ " : " ^ str_of_class_env c_env);
      print_methods c_env.methods;
    ) classes_env


let build_global_env ast verbose =
  (* Key: ref_type; Value: class_env *)
  let classes_env = Env.initial();
  in
  (* load predefined classes, each class has its own class_env *)
  List.iter
    (fun (r_type, cls_env) ->
      Env.add classes_env r_type cls_env; ())
    [ Type.object_type, { parent = {tpath=[]; tid=""}; methods = Env.initial(); attributes = []; };
      { tpath=[]; tid="Integer" }, { parent = Type.object_type; methods = Env.initial(); attributes = []; };
      { tpath=[]; tid="String" }, { parent = Type.object_type; methods = Env.initial(); attributes = []; };
      { tpath=[]; tid="Boolean" }, { parent = Type.object_type; methods = Env.initial(); attributes = []; }; ];

  (*step 1*)
  build_classes_names classes_env ast;

  (*step 2*)
  build_methods classes_env ast;

  if verbose then
    let _= print_endline "--------- env --------";
    in print_classes_env classes_env;

  classes_env

let rec type_params_list classesEnv params = match params with
	| [] -> []
	| t::q -> (match (Located.elem_of t) with
			| Param(c, s) -> Located.mk_elem (TypedParam(c, s, type_of_classname classesEnv (Located.elem_of c) (Located.loc_of c)))
					(Located.loc_of t)::(type_params_list classesEnv q)
		)

let rec type_method_list classesEnv class_curr l =
        let type_method mtype mname=
                TypedMethod(mtype,mname)
        in
        let type_method m static =
                let mtype=Classname(Type.stringOf m.mreturntype)
                and mname=m.mname
                and margstype=m.margstype
                and mbody=m.mbody
                and mthrows=m.mthrows
                in let nparams = type_params_list classesEnv params
                let t_args = mk_t_args m.margstype [] in
                let key = {name=mname; args=t_args} in
                let return_type = type_of_classname class_curr.methods key in
                (*in let ne = type_expr (Some currentClassEnv.name) (not static) classesEnv ((parse_attributes currentClassEnv.attributes)@params_vartypes) mbody*)

                if (static) then TypedStaticMethod(mtype, mname, nparams, return_type)
			else TypedMethod(mtype, mname, nparams, return_type)

        in let typed_method m
                let p a = match a with
                | Static ->  true
                | _ -> false
                in match List.exists p m.amodifiers with
		| true -> type_method m true
		| false -> type_method m false
        in match l with
        | [] -> []
        | t::q -> (typed_method t)::(type_method_list classesEnv class_curr q)

(*margstype : argument list;
    mthrows : Type.ref_type list;
    mbody : statement list;*)

let rec type_attr_list classesEnv class_curr l =
        let typed_attr a=
                let atype=Classname(Type.stringOf a.atype)
                and aname=a.aname
                and amodifiers=a.amodifiers in
		  match a.adefault with
		    | None -> TypedAttr(atype,aname)
                    (*TODO | None -> TypedAttr(aname,atype,type_of_classname classesEnv r_type aname)*)
		    (*TODO| Some e -> type_attr_with_value atype aname e*)

        in match l with
        | [] -> []
        | t::q -> (typed_attr t)::(type_attr_list classesEnv class_curr q)

let typing ast verbose =
  let env = build_global_env ast verbose
  in let rec type_rec_structure_tree sub_tree =
                (* This inner function receives a non-located class_or_expr *)
                let type_structure t=
                        let ref_type={tpath = (trim_option_type ast.package []); tid = t.id}
                        in let class_curr = Env.find env ref_type
                        in match t.info with
                        | Class c -> TypedClassdef(type_attr_list env class_curr c.cattributes,type_method_list env class_curr c.cmethods)
                in match sub_tree with
                | [] -> []
                | t::q -> type_structure t::(type_rec_structure_tree q)
   in
   type_rec_structure_tree ast.type_list

