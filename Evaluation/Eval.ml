open Env
open Type
open AST
open TAST
open Compiling
open EnvType


exception No_Entry_Point
exception NotImplemented of string
exception Action_Not_Supported of string


let heap_size = ref 0

type evaled_expr =
  | EValue of t_value
  | EName of string
  | ERef of int
  | EVoid
  | ENull

let string_of_value v =
  match v with
  | EValue(TInt(i)) -> string_of_int i ^ " (int)"
  | EValue(TFloat(f)) -> string_of_float f ^ " (float)"
  | EValue(TChar(c)) ->
     begin
     match c with
       | None -> "None (char)"
       | Some v -> Char.escaped v ^ " (char)"
     end
  | EValue(TBoolean(b)) -> string_of_bool b ^ " (boolean)"
  | EName (id) -> id
  | ERef (r) -> string_of_int r ^ " (ref)"
  | ENull -> "null"
  | _ -> raise(NotImplemented("string_of_value"))


let print_current_frame frame =
  print_endline "------ current frame ------";
  Env.iter
  (
    fun (id, v) -> print_endline (id ^ " : " ^string_of_value v);
  ) frame


let print_obj_tbl obj_tbl =
  Hashtbl.iter
  (
    fun id v -> print_endline ("  " ^ id ^ " : " ^ string_of_value v);
  ) obj_tbl


let print_heap heap =
  print_endline "=== heap ===";
  Hashtbl.iter
  (
    fun ref_id obj_tbl ->
      print_endline ("[" ^ (string_of_int ref_id) ^ "] :");
      print_obj_tbl obj_tbl
  ) heap


let rec eval_stmt stmt heap frame cls_descs =

  let rec eval_expression e =
    match e.t_edesc with
    | TVal (v, t) -> EValue(v)
    | TOp (e1, op, e2, t) ->
      begin
        let v1 = get_ref e1 frame 
        and v2 = get_ref e2 frame in
        print_endline ("v1, v2 : " ^ string_of_value v1 ^ " , " ^ string_of_value v2);
            match v1, v2 with
            | EValue(TInt(i1)), EValue(TInt(i2)) ->
              let i =
                match op with
                | Op_add -> i1 + i2
                | Op_sub -> i1 - i2
                | Op_mul -> i1 * i2
                | Op_div -> i1 / i2
                | Op_mod -> i1 mod i2
              in print_endline ((string_of_int i1) ^ (string_of_infix_op op) ^ (string_of_int i2) ^ "=" ^ (string_of_int i)) ;
              EValue(TInt(i))
            | EValue(TFloat(f1)), EValue(TInt(i2)) ->
              let f =
                match op with
                | Op_add -> f1 +. float_of_int i2
                | Op_sub -> f1 -. float_of_int i2
                | Op_mul -> f1 *. float_of_int i2
                | Op_div -> f1 /. float_of_int i2
              in print_endline ((string_of_float f1) ^ (string_of_infix_op op) ^ (string_of_int i2) ^ "=" ^ (string_of_float f)) ;
              EValue(TFloat(f))
            | EValue(TInt(i1)), EValue(TFloat(f2)) ->
              let f =
                match op with
                | Op_add -> float_of_int i1 +. f2
                | Op_sub -> float_of_int i1 -. f2
                | Op_mul -> float_of_int i1 *. f2
                | Op_div -> float_of_int i1 /. f2
              in print_endline ((string_of_int i1) ^ (string_of_infix_op op)  ^ (string_of_float f2) ^ "=" ^ (string_of_float f)) ;
              EValue(TFloat(f))
            | EValue(TFloat(f1)), EValue(TFloat(f2)) ->
              let f =
                match op with
                | Op_add -> f1 +. f2
                | Op_sub -> f1 -. f2
                | Op_mul -> f1 *. f2
                | Op_div -> f1 /. f2
              in print_endline ((string_of_float f1) ^ (string_of_infix_op op) ^ (string_of_float f2) ^ "=" ^ (string_of_float f)) ;
              EValue(TFloat(f))
        | _ -> raise(NotImplemented("TOp"))
      end
    | TAssignExp(e1, op, e2, t) ->
      begin
        let variable = eval_expression e1
        and v = eval_expression e2 in
        match op with
        | Assign ->
            print_endline ("+++Assign:" ^ (string_of_value v));
            Env.replace frame (string_of_value variable) v;
            EVoid
      end
    | TName (id, t) -> EName(id)
    | TNew (None, qname, el, Ref(rt)) ->
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

        let ref_id = !heap_size in
        Hashtbl.add heap ref_id obj_tbl;
        heap_size := !heap_size + 1;
        ERef(ref_id)
    | TCall (Some e, mname, arg_list, t) ->
        (* 1. copy the frame
         * 2. find method tast
         * 3. eval the tast, passing a new frame
         * *)
        (* FIXME: not sure ...*)
        let ref = get_ref e frame in
        let new_frame = Env.define frame "this" ref in
        let t = Typing.type_of_typed_expr e in

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
          name = mname;
          args = (construct_arg_list_by_texpr_list arg_list [])
        } in

        let rt =
          begin
            match t with
            | Ref(r) -> r
            | _ -> raise(Action_Not_Supported("cannot call method of primitive type"))
          end in

        let cls_d = Hashtbl.find cls_descs rt in
        let the_method = Hashtbl.find cls_d.c_methods m_sig in
        print_endline ("**Call: " ^ mname);
        eval_method the_method heap new_frame cls_descs;
        print_endline ("**Finished Call: " ^ mname);
        EVoid

    | _ -> raise(NotImplemented("eval_expression"))

    and get_ref e frame =
      let ref = eval_expression e in
      match ref with
      | EName(n) ->
          Env.find frame n
      | _ -> ref


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
            (* TODO create instance *)
            let cls_d = Hashtbl.find cls_descs rt in
            let ref = get_ref e frame in
            Env.add frame id ref; ()
        | Primitive (pt) ->
            (* put the value directely into the current frame *)
            (* FIXME not support short and float type *)
            let v = get_ref e frame in
            Env.add frame id v;
            ()
        | Array (at, i) -> ();
        | _ -> ();
        end
    | (t, id, None) ->
        raise(NotImplemented("eval_var_decl, no expression"));

  in match stmt with
  | TVarDecl vd_list ->
      List.iter (fun vd -> eval_var_decl vd ) vd_list
  | TExpr e -> eval_expression e; ()
  | TReturn(None) ->
    print_endline "TODO: Return statement"; ()
  | TReturn(Some e) ->
  (*
    TODO
    let v = eval_expression e in
    match v with 
    *)
    print_endline "TODO: Return statement"; ()
  | _ -> ()


and eval_method ep heap frame class_descriptors =
  List.iter
  (
    fun stmt ->
      eval_stmt stmt heap frame class_descriptors;
      print_current_frame frame;
      print_heap heap; ()
  ) ep.t_mbody


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

  print_endline ep.t_mname


let eval t_ast class_descriptors =
  (* TODO: return a list of evaluated results *)
  let heap = Hashtbl.create 100
  in
  (* TODO: initilization of static instances *)
  (* TODO: find entry point *)
  let ep = ref {
    t_mmodifiers = []; t_mname = ""; t_mreturntype = Void;
    t_margstype = []; t_mbody = []; t_mthrows = [];
  } in
  find_entry_point t_ast.t_type_list ep;
  print_entry_point !ep;

  let frame = Env.initial();
  in eval_method !ep heap frame class_descriptors;
  ();
