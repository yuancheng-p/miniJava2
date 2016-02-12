open AST
open Type
open EnvType
open Helper

exception Class_Redifinition of string;;
exception Attribute_Redifinition of string;;
exception Method_Signiture of string;;
exception Class_Extends of string;;


(*************  Printer ************)

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

let print_attributes attributes =
  List.iter (
    fun attr ->print_string " attr: "; print_attribute " " attr;
    ) attributes


let print_classes_env classes_env =
  (* get key *)
  let s_keys = Env.sort_hash_key classes_env in
  List.iter (fun r_type -> 
    let c_env = Env.find classes_env r_type in
      print_string
      (str_of_ref_type r_type ^ " : " ^ str_of_class_env c_env);
      print_methods c_env.methods;
      print_attributes c_env.attributes;
    ) s_keys

(***********************)
(**
 *  check if there is a circle when children class extends the parent
 **)
let check_extends_circle classes_env =
  Env.iter (
    fun (ref_type,class_env) -> (*print_endline (" "^ref_type.tid^" p:"^class_env.parent.tid)*)
    let visited_table = Env.initial() in
      let rec check_aclass_extends classes_env visited_table class_key class_env =
        match class_key with
        | { tpath = [_] ; tid = "Object" } -> print_string "find type object";()
        | { tpath = [_] ; tid = "" } -> ()
        | _ -> (
          (* print_endline ("now the type"^class_key.tid); *)
        (List.iter print_string class_env.parent.tpath);
          let isfind = Env.mem visited_table class_key in
          match isfind with
          | true -> raise(Class_Extends("There is a circle with class "^ class_key.tid));()
          | false -> (
            try 
	            (* print_endline ("add to visited_table,the next is"^class_env.parent.tid); *)
	            Env.add visited_table class_key true; 
	            check_aclass_extends classes_env visited_table (class_env.parent) (Env.find classes_env class_env.parent) 
            with Not_found ->())
        )
	   in check_aclass_extends classes_env visited_table ref_type class_env;()
    ) classes_env

let check_class_env classes_env ast =
  check_extends_circle classes_env;
  ()


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
          check_class_redifinition {tpath = [] (*(trim_option_type ast.package [])*); tid = h.id} classes_env;
          (Env.add classes_env
              {tpath = [] (*(trim_option_type ast.package [])*); tid = h.id}
              {parent = c.cparent; methods = Env.initial(); attributes = []});
          parse_asts others
        | Inter -> classes_env
    )
  in parse_asts ast.type_list


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
                {tpath=[](*(trim_option_type ast.package [])*); tid=h.id} c.cmethods;
            iter_asts others
        | Inter -> global_env
    )
  in iter_asts ast.type_list

let build_attrs global_env ast =
   let rec check_attr_redefined attrs a_attr =
    match attrs with
    | [] -> ();
    | h::others ->
      begin
        if h.aname = a_attr.aname then raise(Attribute_Redifinition(a_attr.aname))
        else check_attr_redefined others a_attr
      end
  in
  let rec add_attrs global_env r_type attrs cls_env_alist=
    match attrs with
    | [] ->let cls_env = Env.find global_env r_type in cls_env.attributes <- cls_env_alist;global_env
    | h::others ->
      begin
        let cls_env = Env.find global_env r_type
        in check_attr_redefined cls_env_alist h;
        add_attrs global_env r_type others (h::cls_env_alist)
      end
  (* iterate the ast list *)
  in let rec iter_asts ast_list =
    match ast_list with
    | [] -> global_env
    | h::others -> (match h.info with
        | Class c ->
            add_attrs
                global_env
                {tpath=[](*(trim_option_type ast.package [])*); tid=h.id} c.cattributes [];
            iter_asts others
        | Inter -> global_env
    )
  in iter_asts ast.type_list


let build_global_env ast verbose =
  (* Key: ref_type; Value: class_env *)
  let classes_env = Env.initial();
  in
  (* load predefined classes, each class has its own class_env *)
  List.iter
    (fun (r_type, cls_env) ->
      Env.add classes_env r_type cls_env; ())
    [ Type.object_type, { parent = {tpath=[]; tid=""}; methods = Env.initial(); attributes = []; };
      Type.integer_type, { parent = Type.object_type; methods = Env.initial(); attributes = []; };
      Type.string_type, { parent = Type.object_type; methods = Env.initial(); attributes = []; };
      Type.boolean_type, { parent = Type.object_type; methods = Env.initial(); attributes = []; }; ];

  (*step 1*)
  build_classes_names classes_env ast;

  (*step 2*)
  build_methods classes_env ast;

  (*step 3*)
  build_attrs classes_env ast;

  (*step 4*)
  check_class_env classes_env ast;

  if verbose then
    begin
     print_endline "--------- env --------";
     print_classes_env classes_env;
    end;

  classes_env

