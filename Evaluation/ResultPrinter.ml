open Type
open TAST
open AST

exception No_Entry_Point
exception NotImplemented of string
exception Action_Not_Supported of string
exception Fatal_Error of string


type evaled_expr =
  | EValue of t_value
  | EName of string
  | ERef of int
  | EAttr of int * string (* ex: refer obj by obj_id, static is not considered *)
  | EVoid
  | ENull


exception Return_Val of evaled_expr


type obj = {
  obj_t: Type.t;
  obj_tbl: (string, evaled_expr) Hashtbl.t
}


let string_of_value v =
  match v with
  | EValue(TInt(i)) -> string_of_int i
  | EValue(TFloat(f)) -> string_of_float f
  | EValue(TChar(c)) ->
     begin
     match c with
       | None -> "None (char)"
       | Some v -> Char.escaped v
     end
  | EValue(TBoolean(b)) -> string_of_bool b
  | EName (id) -> id
  | EAttr (obj_id, name) -> "[" ^ string_of_int obj_id ^ "]." ^ name
  | ERef (r) -> string_of_int r ^ " (ref)"
  | ENull -> "null"
  | EVoid -> "void"
  | _ -> raise(NotImplemented("string_of_value"))


let print_current_frame frame =
  print_endline "------ current frame ------";
  Env.iter
  (
    fun (id, v) -> print_endline (id ^ " : " ^string_of_value v);
  ) frame


let print_state_in_frame frame id=
  print_endline ("    ------ " ^ id ^ " in frame ------");
  let v = Env.find frame id in
  print_endline ("    " ^ id ^ " : " ^ string_of_value v)


let print_obj_tbl obj_tbl =
  Hashtbl.iter
  (
    fun id v -> print_endline ("      " ^ id ^ " : " ^ string_of_value v);
  ) obj_tbl


let print_state_in_heap heap ref=
  print_endline ("    ------ object in heap ------");
  let ref_id =
    match ref with
    | ERef(r) -> r in
    let obj = Hashtbl.find heap ref_id in
      print_endline (
        "    [" ^ (Type.stringOf obj.obj_t) ^ "] "
        ^ "(" ^ (string_of_int ref_id) ^ ") :");
      print_obj_tbl obj.obj_tbl


let print_heap heap ref=
  print_endline "=== heap ===";
  Hashtbl.iter
  (
    fun ref_id obj ->
      print_endline (
        "[" ^ (Type.stringOf obj.obj_t) ^ "] "
        ^ "(" ^ (string_of_int ref_id) ^ ") :"
      );
      print_obj_tbl obj.obj_tbl
  ) heap


let rec string_of_expression_desc = function
  | TNew(_,n,al,t) ->
      "new "^(String.concat "." n)^"("^
      (String.concat "," (List.map string_of_expression al))^
      ")"
  | TIf(c,e1,e2) ->
      "if "^(string_of_expression c)^" { "^
      (string_of_expression e1)^" } else { "^(string_of_expression e2)^" }"
  | TCall(r,m,al,t) ->
     (match r with
      | Some r -> (string_of_expression r)^"."
      | None -> "")^
       m^"("^
      (String.concat "," (List.map string_of_expression al))^
	")"
  | TVal(v,t) -> string_of_value (EValue(v))
  | TName(s,t) -> s
  | TAttr(e, id, t) -> (string_of_expression e)^"."^id
  | TAssignExp(e1,op,e2,t) ->
      (string_of_expression e1)^(string_of_assign_op op)^(string_of_expression e2)
  | TOp(e1,op,e2,t) ->
      (string_of_expression e1)^(string_of_infix_op op)^(string_of_expression e2)
  | TPost(e,Incr,t) -> (string_of_expression e)^"++"
  | TPost(e,Decr,t) -> (string_of_expression e)^"--"
  | TPre(op,e,t) -> (string_of_prefix_op op)^(string_of_expression e)
  | TType t -> Type.stringOf t


and string_of_expression e =
  let s = string_of_expression_desc e.t_edesc in
  s
