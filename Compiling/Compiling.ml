open Type
open TAST
open Hashtbl
open EnvType

type cls_descriptor = {
  c_parent: ref_type;
  c_attributes: (string, TAST.t_astattribute) Hashtbl.t;
  c_methods: (EnvType.t_method_signiture, TAST.t_astmethod) Hashtbl.t;
}


(* construct methods args using args in tast form *)
let rec mk_t_t_args t_arguments l =
  match t_arguments with
  | [] -> l
  | h::others ->
      mk_t_t_args others ({tvararg=h.t_vararg;tptype=h.t_ptype}::l)


let create_attr_table  attr_list =
  let tb = Hashtbl.create 30;
  (* TODO: override parent's attributes *)
  in List.iter (* foreach t_astattribute *)
  (
    fun a ->
      Hashtbl.add tb a.t_aname a
  ) attr_list; tb


let create_methods_table method_list =
  (* return a hash table with
   * methods signature as key, method t_ast as value
   * *)
  let tb = Hashtbl.create 30
  in List.iter (* foreach t_astmethod *)
  (
    fun m ->
      let m_sig = {
        name = m.t_mname;
        args = mk_t_t_args m.t_margstype [];
      } in
      Hashtbl.add tb m_sig m
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
             c_parent = t_astcls.t_cparent;
             c_methods = create_methods_table t_astcls.t_cmethods;
             c_attributes = create_attr_table t_astcls.t_cattributes;
           }
           in Hashtbl.add class_descriptors cls_ref descriptor
         end
      | TInter -> ()
  )
  type_list


(* TODO: how to load the predefined methods of a Object, Integer, String etc.? *)
let build_basic_class_descriptors class_descriptors =
  let basic_descs = [
    Type.object_type, {
      c_parent = {tpath=[]; tid=""};
      c_methods = create_methods_table [];
      c_attributes = create_attr_table [];
    };
    Type.integer_type, {
      c_parent = Type.object_type;
      c_methods = create_methods_table [];
      c_attributes = create_attr_table [];
    };
  ] in
  List.iter
  (
    fun (cls_ref, descriptor) ->
    Hashtbl.add class_descriptors cls_ref descriptor; ()
  ) basic_descs


let compile t_ast =

  (* ref_type: class_descriptor *)
  let class_descriptors = Hashtbl.create 10 in
  build_basic_class_descriptors class_descriptors;
  build_class_descriptors t_ast class_descriptors;
  class_descriptors
