open AST
open Type


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
let build_classes_names classes_env ast =
  let rec parse_asts ast_list =
    match ast_list with
    | [] -> classes_env
    | h::others -> (match h.info with
        | Class c ->
          (*TODO: check if class id exist*)
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
  let rec add_methods global_env r_type cmethods =
    match cmethods with
    | [] -> global_env
    | h::others ->
        print_endline ("TODO: check add and methods:"^r_type.tid^":"^h.mname);
        let cls_env = Env.find global_env r_type

        in
        let t_args = mk_t_args h.margstype [] (*TODO inline me*)
        in Env.add cls_env.methods
            {name=h.mname; args=t_args} {modifiers=[];return_type=h.mreturntype};
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

let str_of_methods methods = "TODO:print methods table"

let str_of_class_env class_env =
  "class_env(parent="
      ^(String.concat "." class_env.parent.tpath)^": "^class_env.parent.tid
      ^";methods="^(str_of_methods class_env.methods)
      ^")\n"


let print_classes_env classes_env =
  Env.iter (
    fun (r_type, c_env) ->
      print_string
      (str_of_ref_type r_type ^ " : " ^ str_of_class_env c_env);
    ) classes_env


let build_global_env ast =
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
  print_endline "-------- step 1 ---------";
  build_classes_names classes_env ast;
  print_classes_env classes_env;

  (*step 2*)
  print_endline "-------- step 2 ---------";
  build_methods classes_env ast;
  print_classes_env classes_env


let typing ast =
  let env = build_global_env ast
  in ()

