open Env
open Type
open AST
open TAST
open Compiling
open EnvType
open ResultPrinter

let heap_size = ref 0


let find_obj_in_heap heap obj_id =
  if (Hashtbl.mem heap obj_id) = false then
    raise (Fatal_Error( "object id '" ^ (string_of_int obj_id)
      ^ "' not found in the heap."));
  Hashtbl.find heap obj_id


(* check if an attr exists in the the obj, return the obj if yes *)
let check_obj_attr heap obj_id attr_name =
  let obj = find_obj_in_heap heap obj_id in
  if (Hashtbl.mem obj.obj_tbl attr_name) = false then
    raise (Fatal_Error( "Attribute '" ^ (attr_name)
      ^ "' not found in the obj '" ^ (string_of_int obj_id) ^ "''"));
  obj


let find_attribute_in_obj heap obj_id attr_name =
  let obj = check_obj_attr heap obj_id attr_name in
  Hashtbl.find obj.obj_tbl attr_name


let update_obj heap obj_id attr_name new_v =
  let obj = check_obj_attr heap obj_id attr_name in
  Hashtbl.replace obj.obj_tbl attr_name new_v


let indent = ref false


let rec eval_method ep heap frame class_descriptors =
  try
    eval_stmt_list ep.t_mbody heap frame class_descriptors;
    EVoid (* return void by default *)
  with
  | Return_Val (ret) -> ret
  (* TODO: raise Java Exception *)

and eval_stmt_list sl heap frame class_descriptors =
  match sl with
  | [] -> ()
  | h::others -> begin
      match h with
      | TReturn(_) -> (* stop the eval *)
          let ret = eval_stmt h heap frame class_descriptors in
          raise (Return_Val(ret))
      | _ ->
          eval_stmt h heap frame class_descriptors;
          eval_stmt_list others heap frame class_descriptors
      end


and eval_stmt stmt heap frame cls_descs =

  let rec eval_expression e =
    match e.t_edesc with
    | TVal (v, t) -> EValue(v)
    | TOp (e1, op, e2, t) ->
      begin
        let v1 = deep_eval e1 frame
        and v2 = deep_eval e2 frame in
        let val_of_condition c = if c then EValue(TBoolean(true)) else EValue(TBoolean(false)) in
        match op with
        | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> begin
            match v1, v2 with
            | EValue(TInt(i1)), EValue(TInt(i2)) ->
              let i =
                match op with
                | Op_add -> i1 + i2
                | Op_sub -> i1 - i2
                | Op_mul -> i1 * i2
                | Op_div -> i1 / i2
                | Op_mod -> i1 mod i2
              in EValue(TInt(i))
            | EValue(TFloat(f1)), EValue(TInt(i2)) ->
              let f =
                match op with
                | Op_add -> f1 +. float_of_int i2
                | Op_sub -> f1 -. float_of_int i2
                | Op_mul -> f1 *. float_of_int i2
                | Op_div -> f1 /. float_of_int i2
              in EValue(TFloat(f))
            | EValue(TInt(i1)), EValue(TFloat(f2)) ->
              let f =
                match op with
                | Op_add -> float_of_int i1 +. f2
                | Op_sub -> float_of_int i1 -. f2
                | Op_mul -> float_of_int i1 *. f2
                | Op_div -> float_of_int i1 /. f2
              in EValue(TFloat(f))
            | EValue(TFloat(f1)), EValue(TFloat(f2)) ->
              let f =
                match op with
                | Op_add -> f1 +. f2
                | Op_sub -> f1 -. f2
                | Op_mul -> f1 *. f2
                | Op_div -> f1 /. f2
              in EValue(TFloat(f))
        end
        | Op_eq -> val_of_condition(v1 = v2)
        | Op_ne -> val_of_condition(v1 != v2)
        | Op_gt -> val_of_condition(v1 > v2)
        | Op_lt -> val_of_condition(v1 < v2)
        | Op_ge -> val_of_condition(v1 >= v2)
        | Op_le -> val_of_condition(v1 <= v2)
        | Op_cor ->
            val_of_condition(v1 = EValue(TBoolean(true)) || v2 = EValue(TBoolean(true)))
        | Op_cand ->
            val_of_condition(v1 = EValue(TBoolean(true)) && v2 = EValue(TBoolean(true)))
        | _ -> raise(NotImplemented("operator not supported yet."))
      end
    | TAssignExp(e1, op, e2, t) ->
      begin
        let variable = eval_expression e1
        and v = deep_eval e2 frame in
        match op with
        | Assign ->
            begin
              let _ = match variable with
                | EName (id) -> replace_in_frame_or_heap frame heap id v;
                | EAttr (obj_id, id) ->
                    begin
                      match v with
                      | EAttr(o_id, attr) ->
                          let a = find_attribute_in_obj heap o_id attr in
                          update_obj heap obj_id id a;
                      | _ -> update_obj heap obj_id id v;
                    end
              in EVoid
            end
      end
    | TPost (e, op, t) ->
        let n = eval_expression e
        and v = deep_eval e frame
        in begin
          match n, v with
          | EName(name), EValue(TInt(i)) ->
            begin
              match op with
              | Incr ->
                let new_v = EValue(TInt(i+1))
                in replace_in_frame_or_heap frame heap name new_v; v
              | Decr ->
                let new_v = EValue(TInt(i-1))
                in replace_in_frame_or_heap frame heap name new_v; v
            end
        end
    | TPre (op, e, t) ->
        let n = eval_expression e
        and v = deep_eval e frame
        in begin
          match n, v with
          | EName(name), EValue(TInt(i)) ->
            begin
              match op with
              | Op_neg -> EValue(TInt(-i))
              | Op_incr ->
                let new_v = EValue(TInt(i+1))
                in replace_in_frame_or_heap frame heap name new_v; new_v
              | Op_decr ->
                let new_v = EValue(TInt(i-1))
                in replace_in_frame_or_heap frame heap name new_v; new_v
            end
          | EName(name), EValue(TBoolean(b)) ->
            begin
              match op with
              | Op_not ->
                let new_v =
                  match b with
                  | false -> EValue(TBoolean(true))
                  | true -> EValue(TBoolean(false))
                in new_v
            end
        end
    | TName (id, t) -> EName(id)
    | TNew (_, qname, el, Ref(rt)) ->
        call_constructor qname el rt
    | TCall (Some e, mname, arg_list, t) ->
        let ref = deep_eval e frame in
        call_method mname frame arg_list ref
    | TCall (None, mname, arg_list, t) ->
        (* static is not supported, so find the "this" object  *)
        if Env.mem frame "this" = false then
          raise(Action_Not_Supported(
            "you might be calling a static method, but we cannot support that yet :("));

        let ref = Env.find frame "this" in
        call_method mname frame arg_list ref

    | TAttr (e, id, t) ->
        (* we only suppose e as an object, not a class,
         * static field is not considered.
         * *)
        let v = deep_eval e frame in
        let obj_id = match v with
          | ERef(id) -> id
          | EAttr(o_id, id) -> (* recursive: find the next object *)
              begin
                let next_v = find_attribute_in_obj heap o_id id in
                match next_v with
                | ERef(id) -> id
                | _ -> raise(Fatal_Error("unexpected type."))
              end
          | _ -> raise(Fatal_Error("unexpected type."))
        in EAttr(obj_id, id)

    | _ -> raise(NotImplemented("eval_expression"))

    and call_constructor qname el rt =
        let cls_d = Hashtbl.find cls_descs rt in
        let attrs = cls_d.c_attributes in
        let obj_tbl = Hashtbl.create (Hashtbl.length attrs) in
        (* object initialization *)
        Hashtbl.iter
        (
          fun k v ->
            match v.t_adefault with
            | Some e ->
              Hashtbl.add obj_tbl k (eval_expression e)
            | None ->
              (* assign a default value to the attribute *)
              begin
                match v.t_atype with
                | Primitive(p) ->
                  begin
                    match p with
                    | Int | Short | Long | Byte ->
                      Hashtbl.add obj_tbl k (EValue(TInt(0)))
                    | Double | Float ->
                      Hashtbl.add obj_tbl k (EValue(TFloat(0.0)))
                    | Char ->
                      Hashtbl.add obj_tbl k (EValue(TChar(None)))
                    | Boolean ->
                      Hashtbl.add obj_tbl k (EValue(TBoolean(false)))
                  end
                | Ref(r) ->
                    (* reference type attributes are initialized with null value *)
                    Hashtbl.add obj_tbl k ENull
              end
        ) attrs;
        if Hashtbl.length cls_d.c_constructors = 0 then
          begin
          let ref_id = !heap_size in
          Hashtbl.add heap ref_id {obj_t=Ref(rt);obj_tbl=obj_tbl};
          heap_size := !heap_size + 1;
          ERef(ref_id)
          end
        else
          begin
          let ref_id = !heap_size in
          let cname = List.nth qname ((List.length qname)-1) in
          let m_sig = get_sig cname el in
          let the_method = Hashtbl.find cls_d.c_constructors m_sig in
          let new_frame = get_new_frame frame (ERef(ref_id)) the_method.t_cargstype el in

          let ref_id = !heap_size in
          Hashtbl.add heap ref_id {obj_t=Ref(rt);obj_tbl=obj_tbl};
          heap_size := !heap_size + 1;

          indent := not(!indent);
          print_endline ("    /*");
          print_endline ("    ------ Call Constructor: " ^ cname ^ " ---------");
          eval_stmt_list the_method.t_cbody heap new_frame cls_descs;
          print_endline ("    ------ Finished Call Constructor ------------");
          print_endline ("    */\n");
          indent := not(!indent);

          ERef(ref_id)
          end

    and call_method mname frame arg_list ref =
        (* 1. copy the frame
         * 2. find method tast
         * 3. eval the tast, passing a new frame
         * *)

        (* lookup the runtime type (dynamic binding) *)
        let rt =
          begin
            match ref with
            | ERef(ref_id) ->
                if (Hashtbl.mem heap ref_id) = false then
                  raise (Fatal_Error( "object id '" ^ (string_of_int ref_id)
                    ^ "' not found in the heap."));
                let obj = Hashtbl.find heap ref_id in
                begin
                  match obj.obj_t with
                  | Ref(r) -> r
                end
            | _ -> raise(Action_Not_Supported("cannot call method of primitive type"))
          end in
        let cls_d = Hashtbl.find cls_descs rt in
        let m_sig = get_sig mname arg_list in
        let the_method = Hashtbl.find cls_d.c_methods m_sig in
        let new_frame = get_new_frame frame ref the_method.t_margstype arg_list in

        indent := not(!indent);
        print_endline ("    /*");
        print_endline ("    ------ Call method: " ^ mname ^ "() ---------");
        let ret = eval_method the_method heap new_frame cls_descs in
        print_endline ("    ------ Return: " ^ string_of_value ret ^ " ------------");
        print_endline ("    */\n");
        indent := not(!indent);
        ret

    and get_sig name arg_list =
        let rec construct_arg_list_by_texpr_list el result_list =
          match el with
          | [] -> List.rev result_list
          | h::q ->
            begin
              let targ = Typing.type_of_typed_expr h in
              construct_arg_list_by_texpr_list q
                  (List.append result_list [{tvararg = false;tptype = targ}])
            end
        in

        let m_sig = {
          name = name;
          args = (construct_arg_list_by_texpr_list arg_list [])
        } in
        m_sig

     and get_new_frame frame ref t_argstype arg_list =
       let new_frame = Env.define frame "this" ref in
       (* Prepare the arguments for the new_frame,
           * We don't support vararg for the moment!
           * Therefore, the two lists have the same length.
           * *)
        List.map2
          (
            fun a e ->
              let v = eval_expression e in
              Env.add new_frame a.t_pident v;
          ) t_argstype arg_list;
        new_frame

    (* evaluate an expression, if the result is a reference,
     * then return the reference's value from the current frame *)
    and deep_eval e frame =
      let ref = eval_expression e in
      match ref with
      | EName(n) ->
          if Env.mem frame n then
            Env.find frame n
          else begin
            let ref = Env.find frame "this" in
            let ref_id =
            match ref with
            | ERef(r) -> r in
            let obj = Hashtbl.find heap ref_id in
            Hashtbl.find obj.obj_tbl n
          end
      | _ -> ref

    (* replace a varible, if it is a vrible in frame, update the frame,
     * else if it is an attribute in the heap, update the heap*)
    and replace_in_frame_or_heap frame heap id v =
      if Env.mem frame id then
        Env.replace frame id v
      else
        begin
          let ref = Env.find frame "this" in
          let ref_id =
          match ref with
          | ERef(r) -> r in
          let obj = Hashtbl.find heap ref_id in
          Hashtbl.replace obj.obj_tbl id v
        end;

  (* for understanding local variable initialization,
   * see: http://stackoverflow.com/questions/2187632/why-does-javac-complain-about-not-initialized-variable
   * *)
  in let eval_var_decl vd =
    match vd with
    (* ignore when there is no expression.
     * create that variable if there is an assign operation later on.
     * this is safe because it's already checked during typing.
     * *)
    | (t, id, Some e) ->
        begin
        match t with
        | Ref (rt) ->
            print_endline ((Type.stringOf t) ^ " " ^ id ^ " = " ^ (string_of_expression e) ^ ";");
            let cls_d = Hashtbl.find cls_descs rt in
            let ref = deep_eval e frame in
            Env.add frame id ref;
            print_endline ("    /*");
            print_state_in_frame frame id;
            print_state_in_heap heap ref;
            print_endline ("    */\n");
            ()
        | Primitive (pt) ->
            (* put the value directely into the current frame *)
            (* FIXME not support short and float type *)
            print_endline ((Type.stringOf t) ^ " " ^ id ^ " = " ^ (string_of_expression e) ^ ";");
            let v = deep_eval e frame in
            Env.add frame id v;
            print_endline ("    /*");
            print_state_in_frame frame id;
            print_endline ("    */\n");
            ()
        | Array (at, i) -> ();
        | _ -> ();
        end
    | (t, id, None) ->
        raise(NotImplemented("eval_var_decl, no expression"));

  in match stmt with
  | TVarDecl vd_list ->
      List.iter (fun vd -> eval_var_decl vd ) vd_list; EVoid
  | TExpr e ->
       if !indent then
         print_endline ("    " ^ (string_of_expression e) ^ ";")
       else
         print_endline ((string_of_expression e) ^ ";");
         eval_expression e;
       EVoid
  | TBlock(sl) ->
      let new_frame = Env.copy frame in
      eval_stmt_list sl heap new_frame cls_descs;
      (* update the current frame's data *)
      Env.iter
      (
        fun (k, v) ->
          if Env.mem frame k then
            Env.replace frame k v
      ) new_frame;
      EVoid
  | TIf (e, s, None) ->
      let v = eval_expression e in
      if v = (EValue(TBoolean(true))) then begin
        eval_stmt s heap frame cls_descs; EVoid
      end
      else EVoid
  | TIf (e, s1, Some s2) ->
      let v = eval_expression e in
      if v = (EValue(TBoolean(true))) then begin
        eval_stmt s1 heap frame cls_descs; EVoid
      end
      else begin
        eval_stmt s2 heap frame cls_descs; EVoid
      end
  | TWhile(e, s) ->
      while ((eval_expression e) = EValue(TBoolean(true))) do
        eval_stmt s heap frame cls_descs
      done; EVoid
  | TReturn(Some e) -> deep_eval e frame
  | _ -> EVoid


(* find the main method *)
let rec find_entry_point type_list ep =
  let find_ep_from_cls asttype =
    match asttype.t_info with
    | TClass c ->
        List.iter
        (
          fun m ->
            if m.t_mname = "main" then ep := m
        ) c.t_cmethods

  in match type_list with
  | [] -> ()
  | h::others ->
      find_ep_from_cls h; find_entry_point others ep; ()


(* ep is a t_astmethod *)
let print_entry_point ep =
  if ep.t_mname <> "main" then
    raise(No_Entry_Point);

  print_endline ("_______________" ^ ep.t_mname ^ "___________________")


let eval t_ast class_descriptors =
  let heap = Hashtbl.create 100
  in
  let ep = ref {
    t_mmodifiers = []; t_mname = ""; t_mreturntype = Void;
    t_margstype = []; t_mbody = []; t_mthrows = [];
  } in
  find_entry_point t_ast.t_type_list ep;
  print_entry_point !ep;

  let frame = Env.initial();
  in eval_method !ep heap frame class_descriptors;
  ()
