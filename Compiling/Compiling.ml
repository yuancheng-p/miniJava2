open Type
open TAST
open Hashtbl
open EnvType

type cls_descriptor = {
  c_parent: ref_type;
  c_attributes: (string, TAST.t_astattribute) Hashtbl.t;
  c_methods: (EnvType.t_method_signiture, TAST.t_astmethod) Hashtbl.t;
  c_constructors: (EnvType.t_method_signiture, TAST.t_astconst) Hashtbl.t;
}


(* construct methods args using args in tast form *)
let rec mk_t_t_args t_arguments l =
  match t_arguments with
  | [] -> l
  | h::others ->
      mk_t_t_args others ({tvararg=h.t_vararg;tptype=h.t_ptype}::l)


let create_attr_table  attr_list =
  let tb = Hashtbl.create 30;
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


let create_consts_table constructor_list =
  (* return a hash table with
   * methods signature as key, method t_ast as value
   * *)
  let tb = Hashtbl.create 30
  in List.iter (* foreach t_astmethod *)
  (
    fun m ->
      let m_sig = {
        name = m.t_cname;
        args = mk_t_t_args m.t_cargstype [];
      } in
      Hashtbl.add tb m_sig m
  ) constructor_list; tb


(* load the methods and attributes extends from parents classes, the override is considered *)
let build_class_overload_descriptors t_ast class_descriptors =

  (* create new descriptor loaded the method and attrs *)
  let override_attrs_methods cls_ref class_descriptors new_descriptor =
    let rec rec_overload_attrs_methods from_class_ref new_descriptor =
      match from_class_ref with
      | { tpath = [] ; tid = "Object" } -> new_descriptor
      | _ -> begin
        (* find parent_method *)
        let from_descriptor = Hashtbl.find class_descriptors from_class_ref in
        let from_attributes = from_descriptor.c_attributes in
        let new_attributes = new_descriptor.c_attributes in
        let from_methods = from_descriptor.c_methods in
        let new_methods = new_descriptor.c_methods in
        (* load methods from parent class *)
        (* check each from_method if it is overloaded method, if not, put from_method into the new_descriptor *)
        Hashtbl.iter ( fun from_msigniture from_mast ->
          if Hashtbl.mem new_methods from_msigniture then ()
          else
            Hashtbl.add new_methods from_msigniture from_mast
          ) from_methods;
         (* load attributes from parent class *)
        Hashtbl.iter ( fun from_attrname from_aast ->
          if Hashtbl.mem new_attributes from_attrname then ()
          else
            Hashtbl.add new_attributes from_attrname from_aast
          ) from_attributes;
          rec_overload_attrs_methods from_descriptor.c_parent new_descriptor
      end
    in (* get the parent ref_type and do rec *)
    rec_overload_attrs_methods (let decriptor = Hashtbl.find class_descriptors cls_ref in decriptor.c_parent) new_descriptor in

  (* override descriptors : copy from class_descriptors *)
  let override_descriptors = Hashtbl.copy class_descriptors in
  Hashtbl.iter ( (* for each class descriptor *)
    fun cls_ref descriptor ->
      let new_descriptor = Hashtbl.find override_descriptors cls_ref in
      match cls_ref with
      | { tpath = [] ; tid = "Object" } -> ()
      (* replace the override_descriptor by new_descriptor *)
      | _ -> Hashtbl.replace override_descriptors cls_ref (override_attrs_methods cls_ref class_descriptors new_descriptor) 
    ) class_descriptors; override_descriptors


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
             c_constructors = create_consts_table t_astcls.t_cconsts;
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
      c_constructors = create_consts_table [];
    };
    Type.integer_type, {
      c_parent = Type.object_type;
      c_methods = create_methods_table [];
      c_attributes = create_attr_table [];
      c_constructors = create_consts_table [];
    };
  ] in
  List.iter
  (
    fun (cls_ref, descriptor) ->
    Hashtbl.add class_descriptors cls_ref descriptor; ()
  ) basic_descs


let class_descriptors_printer class_descriptors =
  print_endline "--------- class_descriptors --------";
  Hashtbl.iter ( fun class_ref class_descriptor ->
    print_endline ("class: "^class_ref.tid);
   (* print attrs *)
    let attrs = class_descriptor.c_attributes in
    Hashtbl.iter (
      fun aname aast ->
        print_string "-a-->"; print_endline aname
      ) attrs;
    (* print methods *)
    let methods = class_descriptor.c_methods in
    Hashtbl.iter (
      fun msigniture mast ->
        print_string "-m-->";
        GlobalEnv.print_method_signiture msigniture;print_endline ""
      ) methods
    ) class_descriptors


let compile t_ast =

  (* ref_type: class_descriptor *)
  let class_descriptors = Hashtbl.create 10 in
  build_basic_class_descriptors class_descriptors;
  build_class_descriptors t_ast class_descriptors;
  let overload_descriptiors = build_class_overload_descriptors t_ast class_descriptors in
 (* class_descriptors_printer overload_descriptiors; *)
  overload_descriptiors
