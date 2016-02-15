open AST
open Type
open TAST
open Helper
open EnvType

(* global viriable definition *)
let g_class_ref = ref {tpath=[]; tid=""};;

(* TODO: separate exceptions into a file, handle the error *)
exception Null_Not_Allowed of string
exception NotImplemented
exception SyntaxError
exception Method_Local_Variable_Redefined of string
exception UnknownType of string
exception Variable_Not_Defined of string
exception Type_Mismatch of string

(* to know if ref_type1 is parent of ref_type2 *)
let rec is_parent_of env r1 r2 =
  (* r1 unchange , find r2 -> class_env -> parent *)
  match r1 with
  | { tpath = [] ; tid = "Object" } -> true
  | _ -> begin
    match r2 with
    | { tpath = [] ; tid = "Object" } -> false
    | _ -> let class_env_r2 = Env.find env r2 in
      if r1.tid = class_env_r2.parent.tid then true
      else is_parent_of env r1 class_env_r2.parent
    end

let type_of_typed_expr t_e =
  match t_e.t_edesc with
  | TOp(e1, op, e2, t) -> t
  | TVal(v) -> begin
      match v with
      | TInt i -> Primitive(Type.Int)
      | TFloat f -> Primitive(Type.Float)
      | TChar c -> Primitive(Type.Char)
      | TBoolean b -> Primitive(Type.Boolean)
      | TString s -> Ref(Type.string_type)
      | TNull -> Ref(Type.null_type)
      end
  | TNew(n, qname, params, t) -> t (*Ref({tpath=[];tid=List.hd (List.rev qname)})*) 
  | TAssignExp(e1,assign_op,e2,t) -> t
  | TName(id, t) -> t
  | _ -> print_endline "NotImplemented"; raise(NotImplemented)


(* literal types *)
let type_value v =
  match v with
  | AST.Int i -> TInt(int_of_string i)
  | AST.Float f -> TFloat(float_of_string f)
  | AST.String s -> TString(s)
  | AST.Char c -> TChar(c)
  | AST.Boolean b -> TBoolean(b)
  | AST.Null -> TNull


let rec type_expression_desc env method_env edesc =
  match edesc with
  (* TODO: check and type all the expressions here *)
  | Val v -> TVal(type_value v)
  | Op(e1, op, e2) -> type_op env method_env e1 e2 op
  | New(n,t,params) -> type_new env method_env n t params
  | Name id -> TName(id,(type_of_name env method_env id))
  | AssignExp(e1,assign_op,e2) -> type_assign_exp env method_env e1 assign_op e2
  | _ -> TVoidClass (* a small cheat to avoid Match_failure *)


and type_op env method_env e1 e2 op =
  let typed_e1 = type_expression env method_env e1;
  in let typed_e2 = type_expression env method_env e2;
  in let t1 = type_of_typed_expr typed_e1;
  in let t2 = type_of_typed_expr typed_e2;
  in match t1, t2 with
    | Primitive(p1), Primitive(p2)
    -> begin
      (*TODO: more complexe types check *)
         match p1, p2 with
         | Int, Int
         -> TOp(typed_e1, op, typed_e2, Primitive(Type.Int))
         | Float, _
         -> TOp(typed_e1, op, typed_e2, Primitive(Type.Double))
         | _, Float
         -> TOp(typed_e1, op, typed_e2, Primitive(Type.Double))
         | Float, Float
         -> TOp(typed_e1, op, typed_e2, Primitive(Type.Double))
         | _, _
         -> print_endline "not implemented"; raise(NotImplemented)
       end

    | Ref(r1), Ref(r2)
    -> begin
         (* String only supports Op_add *)
         match r1, r2 with
         | { tpath = []; tid = "String" },
           { tpath = []; tid = "String" }
         -> begin
              match op with
              | Op_add -> TOp(typed_e1, op, typed_e2, Ref(Type.string_type))
              | _
              -> print_endline "action not supported"; raise(NotImplemented)
            end
         | _, _
         -> print_endline "action not supported"; raise(NotImplemented)
       end

    | _, _ -> print_endline "not implemented"; raise(NotImplemented)



and type_expression env method_env e =
  {
    t_edesc = type_expression_desc env method_env e.edesc;
  }


and type_new env method_env n t params=
  let find_ref_type_in_env ttid = 
    if Env.mem env { tpath = [] ; tid = ttid } then
      { tpath = [] ; tid = ttid }
    else
      raise(UnknownType(ttid))
  in let rec check_qname_in_env qname= (* use to ignore package name *)
    match qname with
    | [last] -> find_ref_type_in_env last
    | h::others -> check_qname_in_env others
  in let rec t_expression_desc_list exprs l =
    match exprs with
    | [] -> List.rev l
    | h::others -> type_expression env method_env h::l
  in match (n, t, params) with
  | (None, qname, params)->
    check_qname_in_env qname;
    (* check qname exist in env *)
    TNew(None, qname, t_expression_desc_list params [],
         Ref({tpath=[]; tid=List.hd (List.rev qname)}))
  | (Some name, qname, params) ->
    (* we ignore the package path, find the ref_type only by id *)
    TNew(Some name, qname, t_expression_desc_list params [],
         Ref({tpath=[]; tid=List.hd (List.rev qname)}))

(* Find type of a variable or field *)
and type_of_name env method_env id =
  if Env.mem method_env id then
    begin
      let t = Env.find method_env id
      in t
    end
  else
    begin
    (* find field in class_env->attributes->aname *)
    let class_env = Env.find env g_class_ref.contents in
    let rec has_field attrs id =
      match attrs with
      | [] -> raise (Variable_Not_Defined(id))
      | h::others -> if h.aname=id then h.atype else has_field others id
    in has_field class_env.attributes id
    end

(* compare type of e1 and e2, return the type of e1 , the extends is considered*)
and type_assign_exp env method_env e1 assign_op e2 =
  let typed_e1 = type_expression env method_env e1
  and typed_e2 = type_expression env method_env e2
  in let t1 = type_of_typed_expr typed_e1
  and t2 = type_of_typed_expr typed_e2 in
  if t1 = t2 then TAssignExp(typed_e1,assign_op,typed_e2,t1)
  else
    (* TODO add some other primitive type support *)
    match t1,t2 with
    | Primitive(p1), Primitive(p2) -> begin
      match p1,p2 with
      | Float,Int -> TAssignExp(typed_e1,assign_op,typed_e2,t1)
      | Float,Char -> TAssignExp(typed_e1,assign_op,typed_e2,t1)
      | _, _ -> raise(Type_Mismatch(Type.stringOf t1^" and "^Type.stringOf t2))
      end (* the real type of object can only be known during the run time *)
    | Ref(r1), Ref(r2) -> if is_parent_of env r1 r2 then TAssignExp(typed_e1,assign_op,typed_e2,t1) 
    else 
      raise(Type_Mismatch(Type.stringOf t1^" and "^Type.stringOf t2))

(* check if a ref type is existe in global_env*)
let check_type_ref_in_env t id env = 
  match t with
  (*| Primitive prim ->  *)
  | Ref ref_t ->
    if Env.mem env ref_t = false then
      raise(UnknownType(stringOf_ref ref_t^"->"^id))
  | _ -> () (* TODO Array Type.t.Ref*)

let rec type_var_decl_list env method_env vd_list =

  let check_method_local_variable_redefined method_env id =
    if Env.mem method_env id then
      raise(Method_Local_Variable_Redefined("method local variable redefined : "^id))

  in let type_var_decl env method_env vd =
    match vd with
    | (t, id, Some e) ->
        check_type_ref_in_env t id env;
        let typed_e = type_expression env method_env e
        in let t1 = type_of_typed_expr typed_e
        in if (t = t1) then begin
          check_method_local_variable_redefined method_env id;
          Env.add method_env id t1;
          (t1, id, Some (typed_e))
          end
        else begin
          print_endline "type not match in var_decl";
          raise(SyntaxError)
          end
    | (t, id, None) -> begin
      check_type_ref_in_env t id env;
      check_method_local_variable_redefined method_env id;
      Env.add method_env id t;
      (t, id, None)
      end

  in match vd_list with
  | [] -> []
  | h::others -> type_var_decl env method_env h::(type_var_decl_list env method_env others)


let rec type_statement_list env method_env l =
  let type_statment stmt =
    match stmt with
    | VarDecl vd_list -> TVarDecl(type_var_decl_list env method_env vd_list)
    | Expr e -> TExpr(type_expression env method_env e)
    | _ -> TNop (* a small cheat to avoid Match_failure *)
    (*TODO: check and type all the statments here *)

  in match l with
    | [] -> []
    | h::others -> (type_statment h)::(type_statement_list env method_env others)


let rec type_method_list env l =
  let typed_method m =
    (* method_env with key: variable_name, value: variable_type (which is not important)
     * it is used for local variable redifinition check.
     * *)
    let method_env = Env.initial();
    in {
      t_mbody = type_statement_list env method_env (List.rev m.mbody); (* FIXME *)
      t_mreturntype = m.mreturntype;

      (* TODO: t_mname, t_margstype, t_mthrows *)
    }

  in match l with
     | [] -> []
     | t::q -> (typed_method t)::(type_method_list env q)


let typing ast verbose =
  let env = GlobalEnv.build_global_env ast verbose

  (* for each AST, turn it into TAST *)
  in let rec type_type_list type_list =

    let type_asttype asttype =
      g_class_ref:= {tpath=[];tid=asttype.id};
      let type_type_info t =
        match t.info with
        | Class c ->
            TClass({
              t_cmethods = type_method_list env (List.rev c.cmethods) (* FIXME *)
            })
      in {
        t_modifiers = asttype.modifiers;
        t_id = asttype.id;
        t_info = type_type_info asttype;
      }

    in match type_list with
    | [] -> []
    | h::others -> type_asttype h::(type_type_list others)

  in {
    t_package = ast.package;
    (* FIXME: the type list is parsed inversely during the recursion. *)
    t_type_list = type_type_list (List.rev ast.type_list)
  }
