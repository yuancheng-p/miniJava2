open Type
open TAST


exception No_Entry_Point


let rec eval_stmt stmt heap frame cls_descs =

  let rec eval_expression e =
    match e.t_edesc with
    | TVal (v, t) -> v
    (* | TNew
     * | TCall
     * *)

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
        match t with
        | Ref (rt) ->
            (* TODO create instance *)
            let cls_d = Hashtbl.find cls_descs rt;
            in let instance = eval_expression e;
            in Hashtbl.add heap id instance; ()
        | Primitive (pt) ->
            (* put the value directely into the current frame *)
            let v = eval_expression e;
            in Hashtbl.add frame id v;
            ()
        | Array (at, i) -> ();
        | _ -> ();

  in match stmt with
  | TVarDecl vd_list ->
      List.iter (fun vd -> eval_var_decl vd ) vd_list
  | TExpr e -> eval_expression e; ()
  | _ -> ()


let eval_entry_point ep heap frame class_descriptors =
  List.iter
  (
    fun stmt -> eval_stmt stmt heap frame class_descriptors; ()
  ) ep.t_mbody


(* find the main method *)
let rec find_entry_point type_list =
  let ep = ref {
    t_mmodifiers = []; t_mname = ""; t_mreturntype = Void;
    t_margstype = []; t_mbody = []; t_mthrows = [];
  }
  in let find_ep_from_cls asttype =
    match asttype.t_info with
    | TClass c ->
        List.iter
        (
          fun m ->
            if m.t_mname = "main" then ep := m
        ) c.t_cmethods

  in match type_list with
  | [] -> !ep
  | h::others ->
      find_ep_from_cls h; find_entry_point others; !ep


(* ep is a t_astmethod *)
let print_entry_point ep =
  if ep.t_mname = "" then
    raise(No_Entry_Point);

  print_endline ep.t_mname


let eval t_ast class_descriptors =
  (* TODO: return a list of evaluated results *)
  let heap = Hashtbl.create 100
  in
  (* TODO: initilization of static instances *)

  (* TODO: find entry point *)
  let ep = find_entry_point t_ast.t_type_list;
  in print_entry_point ep;

  let frame = Hashtbl.create 1;
  in eval_entry_point ep heap frame class_descriptors;
  ();
