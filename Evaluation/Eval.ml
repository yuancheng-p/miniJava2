open Type
open TAST
open Compiling


exception No_Entry_Point
exception NotImplemented of string


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
  | EName (id) -> id
  | ERef (r) -> string_of_int r ^ " (ref)"
  | ENull -> "null"
  | _ -> raise(NotImplemented("string_of_value"))


let print_current_frame frame =
  print_endline "------ current frame ------";
  Hashtbl.iter
  (
    fun id v -> print_endline (id ^ " : " ^string_of_value v);
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
        let v1 = eval_expression e1
        and v2 = eval_expression e2 in
        match op with
        | Op_add ->
            match v1, v2 with
            | EValue(TInt(i1)), EValue(TInt(i2)) ->
              let i = i1 + i2 in
              print_endline ((string_of_int i1) ^ "+" ^ (string_of_int i2) ^ "=" ^ (string_of_int i)) ;
              EValue(TInt(i))
        | _ -> raise(NotImplemented("TOp"))
      end
    | TAssignExp(e1, op, e2, t) ->
      begin
        let variable = eval_expression e1
        and v = eval_expression e2 in
        match op with
        | Assign ->
            print_endline ("+++Assign:" ^ (string_of_value v));
            Hashtbl.replace frame (string_of_value variable) v;
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
                    | Int ->
                      Hashtbl.add obj_tbl k (EValue(TInt(0)))
                    (*TODO Double, Boolean, etc. *)
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
    | _ -> raise(NotImplemented("eval_expression"))

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
            let cls_d = Hashtbl.find cls_descs rt;
            in let ref = eval_expression e;
            in let ref =
            match ref with
            | EName(n) ->
                Hashtbl.find frame n;
            | _ ->
                ref;
            in Hashtbl.add frame id ref; ()
        | Primitive (pt) ->
            (* put the value directely into the current frame *)
            let v = eval_expression e in
            Hashtbl.add frame id v;
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
  | _ -> ()


let eval_entry_point ep heap frame class_descriptors =
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

  let frame = Hashtbl.create 1;
  in eval_entry_point !ep heap frame class_descriptors;
  ();
