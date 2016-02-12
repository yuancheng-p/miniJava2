open Type
open TAST
open Hashtbl

type cls_descriptor = {
  c_parent: ref_type;
(* TODO: c_attr: string list; *)
  c_methods: (string, string) Hashtbl.t;
  (* TODO: what are the types of methods ? *)
}


let create_methods cls_ref method_list =
  (* return a hash table with methods names *)
  let tb = Hashtbl.create 30
  in List.iter (* foreach t_astmethod *)
  (
    fun m -> (* TODO: how to distinguish methods with diff # of args ? *)
      ()
  ) method_list; tb


let build_class_descriptors t_ast class_descriptors methods_table =
  let type_list = t_ast.t_type_list
  in List.iter
  (
    fun t ->
      match t.t_info with
      | TClass t_astcls ->
         begin
           let cls_ref = { tpath = []; tid = t.t_id }
           in let cmethods = create_methods cls_ref t_astcls.t_cmethods
           in let descriptor = {
             c_parent = Type.object_type; (* TODO replace by real parent*)
             (* c_parent = t_astcls.t_cparent *)
             c_methods = cmethods
           }
           in Hashtbl.add class_descriptors cls_ref descriptor
         end
      | TInter -> ()
  )
  type_list


let compile t_ast =

  (* ref_type: class_descriptor *)
  let class_descriptors = Hashtbl.create 10
  and methods_table = Hashtbl.create 30
  in build_class_descriptors t_ast class_descriptors  methods_table;

  class_descriptors, methods_table;
