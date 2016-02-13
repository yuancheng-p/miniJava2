open Type
open TAST
open Hashtbl

type cls_descriptor = {
  c_parent: ref_type;
(* TODO: c_attr: string list; *)
  (* for the moment we use the name of the method for the key,
   * please replace me by the full signature of the method.
   * *)
  c_methods: (string, TAST.t_astmethod) Hashtbl.t;
(*  c_methods: (EnvType.t_method_signiture, TAST.t_astmethod) Hashtbl.t; *)
}


let create_methods_table method_list =
  (* return a hash table with
   * methods signature as key, method t_ast as value
   * *)
  let tb = Hashtbl.create 30
  in List.iter (* foreach t_astmethod *)
  (
    fun m ->
      (* TODO: identify method by its name and t_margstype (signature)
       * in order to distinguish methods with different args
       * *)
      Hashtbl.add tb m.t_mname m
  ) method_list; tb


let build_class_descriptors t_ast class_descriptors =
  let type_list = t_ast.t_type_list
  in List.iter
  (
    fun t ->
      match t.t_info with
      | TClass t_astcls ->
         begin
           let cls_ref = { tpath = []; tid = t.t_id }
           in let descriptor = {
             c_parent = Type.object_type; (* TODO replace by real parent*)
             (* c_parent = t_astcls.t_cparent *)
             c_methods = create_methods_table t_astcls.t_cmethods
           }
           in Hashtbl.add class_descriptors cls_ref descriptor
         end
      | TInter -> ()
  )
  type_list


let compile t_ast =

  (* ref_type: class_descriptor *)
  let class_descriptors = Hashtbl.create 10
  in build_class_descriptors t_ast class_descriptors;
  class_descriptors
