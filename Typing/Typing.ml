open AST
open Type


type class_env = {
  parent: ref_type;
  methods: string list; (* TODO replace me by methods list*)
  attributes: string list; (* TODO replace me by attributes list *)
}


(* TODO add package *)
let rec build_classes_names classes_env asts =
  match asts with
  | [] -> classes_env
  | h::others -> (match h.info with
      | Class c ->
        (Env.add classes_env
            {tpath = []; tid = h.id}
            {parent = c.cparent; methods = []; attributes = []});
        build_classes_names classes_env others
      | Inter -> classes_env
  )


let str_of_ref_type ref_type = ref_type.tid


let str_of_class_env class_env = "class_env(parent="^class_env.parent.tid^")\n"


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
    [ { tpath=[]; tid="Object" }, { parent = {tpath=[]; tid=""}; methods = []; attributes = []; };
      { tpath=[]; tid="Integer" }, { parent = Type.object_type; methods = []; attributes = []; };
      { tpath=[]; tid="String" }, { parent = Type.object_type; methods = []; attributes = []; };
      { tpath=[]; tid="Boolean" }, { parent = Type.object_type; methods = []; attributes = []; }; ];
  (*step 1*)
  build_classes_names classes_env ast.type_list;
  print_classes_env classes_env


let typing ast =
  let env = build_global_env ast
  in ()

