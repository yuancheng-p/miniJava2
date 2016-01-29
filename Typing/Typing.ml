open AST
open Type


let type_single_ast ast =
  (match ast.info with
   | Class c -> print_endline "class found"
   | Inter -> print_endline "inner part")

type class_env = {
  parent: ref_type;
  methods: string list; (* TODO replace me by methods list*)
  attributes: string list; (* TODO replace me by attributes list *)
}

let build_global_env ast =
  (* Key: ref_type; Value: class_env *)
  let classes_env = Env.initial();

  in

  (*each class has its own env*)

  List.iter
    (fun (r_type, cls_env) ->
      Env.add classes_env r_type cls_env; ())
    [ {tpath=[]; tid="Object"}, {
        parent = {tpath=[]; tid=""};
        methods = []; attributes = [];
      };
      {tpath=[]; tid="Integer"}, {
        parent = Type.object_type;
        methods = []; attributes = [];
      };
    ];


  (* TODO: delete me, this code is just for testing ...*)
  let int_cls_env = Env.find classes_env {tpath=[]; tid="Integer"};
  in
  print_string "the parent of Integer is:";
  print_endline int_cls_env.parent.tid;

  (* TODO: delete me, just for testing ...*)
  List.iter (fun tree -> type_single_ast tree;) ast.type_list


let typing ast =
  let env = build_global_env ast
  in ()

