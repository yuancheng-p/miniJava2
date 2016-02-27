open AST
open Type
open TAST
open Helper
open EnvType

(* global viriable definition *)
let g_class_ref = ref {tpath=[]; tid=""};;

(* TODO: separate exceptions into a file, handle the error *)
exception Null_Not_Allowed of string
exception Action_Not_Supported of string
exception NotImplemented of string
exception SyntaxError of string
exception Method_Local_Variable_Redefined of string
exception UnknownType of string
exception Variable_Not_Defined of string
exception Type_Mismatch of string
exception Method_Not_exist of string


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


let raise_type_mismatch t1 t2 =
  raise(Type_Mismatch(Type.stringOf t1^" and "^ Type.stringOf t2))


(* check if type t1 is compatible with type t2.
 * return t1 if compatible, raise Type_Mismatch exception otherwise.
 * *)
let check_assignment_type env t1 t2 =
  if t1 = t2 then t1
  else
    match t1, t2 with
    | Ref(r1), Ref(r2) ->
      (* the real type of object can only be known during the runtime *)
      if is_parent_of env r1 r2 then
        t1
      else
        raise_type_mismatch t1 t2
    | Primitive(p1), Primitive(p2) -> begin
      match p1 with
      | Double -> begin match p2 with
        | Boolean -> raise_type_mismatch t1 t2
        | _ -> t1
        end
      | Float -> begin match p2 with
        | Boolean | Double -> raise_type_mismatch t1 t2
        | _ -> t1
        end
      | Long -> begin match p2 with
        | Boolean | Double | Float -> raise_type_mismatch t1 t2
        | _ -> t1
        end
      | Int -> begin match p2 with
        | Boolean | Double | Float | Long -> raise_type_mismatch t1 t2
        | _ -> t1
        end
      | Short -> begin match p2 with
        | Short | Byte -> t1
        | _ -> raise_type_mismatch t1 t2
        end
      | Byte -> begin match p2 with
        | Byte -> t1
        | _ -> raise_type_mismatch t1 t2
        end
      | Boolean -> begin match p2 with
        | Boolean -> t1
        | _ -> raise_type_mismatch t1 t2
        end
      | Char -> begin match p2 with
        | Char -> t1
        | _ -> raise_type_mismatch t1 t2
        end
      end
    (* TODO: boxing and unboxing *)
    | _, _ -> raise_type_mismatch t1 t2


let type_of_typed_expr t_e =
  match t_e.t_edesc with
  | TOp(e1, op, e2, t) -> t
  | TVal(v, t) -> t
  | TNew(n, qname, params, t) -> t (*Ref({tpath=[];tid=List.hd (List.rev qname)})*) 
  | TAssignExp(e1,assign_op,e2,t) -> t
  | TName(id, t) -> t
  | TAttr(e, s, t) -> t
  | TCall(eo, s, el, t) -> t
  | TPost(e, postop , t) ->t
  | TPre(preop, e , t) ->t
  | _ -> raise(NotImplemented("type_of_typed_expr"))


(* literal types *)
let type_value v =
  match v with
  | AST.Int i -> TVal(TInt(int_of_string i), Primitive(Type.Int))
  | AST.Float f -> TVal(TFloat(float_of_string f), Primitive(Type.Float))
  | AST.String s -> TVal(TString(s), Ref(Type.string_type))
  | AST.Char c -> TVal(TChar(c), Primitive(Type.Char))
  | AST.Boolean b -> TVal(TBoolean(b), Primitive(Type.Boolean))
  | AST.Null -> TVal(TNull, Ref(Type.null_type))


let rec type_expression_desc env method_env edesc =
  match edesc with
  (* TODO: check and type all the expressions here *)
  | Val v -> type_value v
  | Op(e1, op, e2) -> type_op env method_env e1 e2 op
  | New(n,t,params) -> type_new env method_env n t params
  | Name id -> TName(id,(type_of_name env method_env g_class_ref.contents id))
  | AssignExp(e1,assign_op,e2) -> type_assign_exp env method_env e1 assign_op e2
  | Attr(e, s) -> type_e_attr env method_env e s
  | Call(eo, s, el) -> type_e_call env method_env eo s el
  | Post(e, postop) -> type_e_post env method_env e postop
  | Pre(preop, e) -> type_e_pre env method_env preop e
  | _ -> TVoidClass (* a small cheat to avoid Match_failure *)


and type_op env method_env e1 e2 op =
  let typed_e1 = type_expression env method_env e1;
  in let typed_e2 = type_expression env method_env e2;
  in let t1 = type_of_typed_expr typed_e1;
  in let t2 = type_of_typed_expr typed_e2;
  in match t1, t2 with
    | Primitive(p1), Primitive(p2)
    -> begin
        match op with
        | Op_add | Op_sub | Op_mul | Op_div | Op_mod -> begin
          if p1 = p2 then
            TOp(typed_e1, op, typed_e2, Primitive(p1))
          else begin
            match p1, p2 with
            | Float, _ | _, Float
            | Double, _ | _, Double ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Double))
            | Long, _ | _, Long ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Long))
            | Int, _ | _, Int ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Int))
            | Short, _ | _, Short ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Short))
            | Char, _ | _, Char ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Char))
            | Byte, _ | _, Byte ->
                TOp(typed_e1, op, typed_e2, Primitive(Type.Byte))
            | _, _ -> raise(SyntaxError(
              "unsupported type for calculation operators."))
          end
        end
        | Op_eq | Op_ne | Op_gt | Op_lt | Op_ge | Op_le -> begin
          match p1, p2 with
          | Boolean, _ | _, Boolean ->
            raise (SyntaxError(
                "comparation operators are undefined for boolean"))
          | _, _ -> TOp(typed_e1, op, typed_e2, Primitive(Type.Boolean))
        end
        | Op_cor | Op_cand -> begin
          match p1, p2 with
          | Boolean, Boolean ->
            TOp(typed_e1, op, typed_e2, Primitive(Type.Boolean))
          | _, _ ->
            raise (SyntaxError(
                "'&&, ||' operators are undefined for non-boolean"))
        end
        | _ -> raise(NotImplemented("unsupported operator."))

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
              -> raise(Action_Not_Supported("string only support add operation"))
            end
         | _, _
         -> raise(Action_Not_Supported("ref_type does not support operators"))
       end

    | _, _ -> raise(NotImplemented("type_op"))



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

(* Find type of a variable or field
 *
 * 1. When this function is used for finding variables in method,
 * we must provide a method_env where we put variables.
 * 2. When it's used for finding names during attributes declaration,
 * we must keep the method_env empty.
 * 3. class_ref indicate to find in which class
 * 4. 'this' support
 * *)
and type_of_name env method_env class_ref id =
  if id = "this" then Ref(class_ref) else
  if Env.mem method_env id then
    begin
      let t = Env.find method_env id
      in t
    end
  else
    begin
    (* find field in class_env->attributes->aname *)
    let class_env = Env.find env class_ref in
    let rec has_field attrs id =
      match attrs with
      | [] -> raise (Variable_Not_Defined(id))
      | h::others -> if h.aname=id then h.atype else has_field others id
    in has_field class_env.attributes id
    end



and type_assign_exp env method_env e1 assign_op e2 =
  let typed_e1 = type_expression env method_env e1
  and typed_e2 = type_expression env method_env e2
  in let t1 = type_of_typed_expr typed_e1
  and t2 = type_of_typed_expr typed_e2 in
  let t = check_assignment_type env t1 t2
  in TAssignExp(typed_e1, assign_op, typed_e2, t)


and type_e_attr env method_env e s =
  let typed_e1 = type_expression env method_env e in
  let t1 = type_of_typed_expr typed_e1 in
  match t1 with
  (* find a attribute in previous class *)
  | Ref(r) -> let ta = type_of_name env (Env.initial()) r s in TAttr(typed_e1, s, ta)
  | Primitive(p) -> TAttr(typed_e1, s, Primitive(p))
  | _ -> raise(NotImplemented("type_e_attr"))
  (* TODO check attrbute modifier private? *)

(* find and check method call from this class or other class *)
(* TODO need to support parent method *)
and type_e_call env method_env eo s el =
  (* construct a argument list from expression list -> same as EnvType.t_arg -> *)
  (* use for find method by signiture in global_env *)
  let rec construct_arg_list_by_expr el result_list =
    match el with
    | [] -> List.rev result_list
    | h::q -> begin
      let typed_arg = type_expression env method_env h in
      let targ = type_of_typed_expr typed_arg in
      construct_arg_list_by_expr q (List.append result_list [{tvararg = false;tptype = targ}])
      end
   in
  let rec typed_arg_list el result_list =
    match el with
    | [] -> List.rev result_list
    | h::q -> begin
      let typed_arg = type_expression env method_env h in
      typed_arg_list q (List.append result_list [typed_arg])
      end
   in
  (* make method signiture -> used to find method in class_env *)
  let method_signiture = {name = s; args = (construct_arg_list_by_expr el [])} in
  match eo with
  (* call method from other reference class *)
  | Some e ->
    begin
      let typed_e1 = type_expression env method_env e in
      let t1 = type_of_typed_expr typed_e1 in
      match t1 with
      | Ref(r) ->
        begin
          let class_env = Env.find env r in
          if Env.mem class_env.methods method_signiture then
            let the_method = Env.find class_env.methods method_signiture in
            TCall(Some(typed_e1), s, typed_arg_list el [], the_method.return_type)
            else raise(Method_Not_exist(s))
        end
      | _ -> raise(NotImplemented("type_e_call"))
    end
   (* method call in same class, find method by name *)
  | None ->
    let class_env = Env.find env g_class_ref.contents in
    if Env.mem class_env.methods method_signiture then
      let the_method = Env.find class_env.methods method_signiture in
      TCall(None, s, typed_arg_list el [], the_method.return_type)
      else raise(Method_Not_exist(s))


(* the primitive type excepte boolean, the others can use post operation '++','--' *)
and type_e_post env method_env e postop =
  let typed_e1 = type_expression env method_env e in
  let t1 = type_of_typed_expr typed_e1 in
  match t1 with
  | Primitive(p) ->
    begin
      match p with
      | Long -> TPost(typed_e1, postop, t1)
      | Int -> TPost(typed_e1, postop, t1)
      | Float -> TPost(typed_e1, postop, t1)
      | Short -> TPost(typed_e1, postop, t1)
      | Double -> TPost(typed_e1, postop, t1)
      | Char -> TPost(typed_e1, postop, t1)
      | Byte -> TPost(typed_e1, postop, t1)
      | _ -> raise(Type_Mismatch(Type.stringOf t1 ^" can not convert to int"))
    end
  | _ -> raise(Type_Mismatch(Type.stringOf t1 ^" can not convert to int"))


(* boolean use pre operation '!' '~' question: '~' is not support for in eclipse java *)
and type_e_pre env method_env preop e =
  let typed_e1 = type_expression env method_env e in
  let t1 = type_of_typed_expr typed_e1 in
  match t1 with
  | Primitive(p) ->
    begin
      match p with
      | Boolean -> TPre(preop, typed_e1, t1)
      | _ -> raise(Type_Mismatch(Type.stringOf t1 ^" can not convert to boolean"))
    end
  | _ -> raise(Type_Mismatch(Type.stringOf t1 ^" can not convert to boolean"))

(* check if a ref type exists in global_env*)
let check_type_ref_in_env t id env = 
  match t with
  (*| Primitive prim ->  *)
  | Ref ref_t ->
    if Env.mem env ref_t = false then
      raise(UnknownType(stringOf_ref ref_t^"->"^id))
  | _ -> () (* TODO Array Type.t.Ref*)

let rec type_var_decl_list env method_env vd_list =
  (* check if a local variable already existes in method_env including method arguments*)
  let check_method_local_variable_redefined method_env id =
    if Env.mem method_env id then
      raise(Method_Local_Variable_Redefined("duplicated local variable : "^id))

  in let type_var_decl env method_env vd =
    match vd with
    | (t, id, Some e) ->
        check_type_ref_in_env t id env;
        let typed_e = type_expression env method_env e in
        let t1 = type_of_typed_expr typed_e in
        check_assignment_type env t t1;
        check_method_local_variable_redefined method_env id;
        Env.add method_env id t;
        (t, id, Some (typed_e))
    | (t, id, None) -> begin
      check_type_ref_in_env t id env;
      check_method_local_variable_redefined method_env id;
      Env.add method_env id t;
      (t, id, None)
      end

  in match vd_list with
  | [] -> []
  | h::others -> type_var_decl env method_env h::(type_var_decl_list env method_env others)


(* is_call determines if the statment list is a method,
 * if true, then the mreturntype is taken into account *)
let rec type_statement_list env method_env l is_call mreturntype =
  let rec type_statment stmt =
    match stmt with
    | VarDecl vd_list -> TVarDecl(type_var_decl_list env method_env vd_list)
    | Expr e -> TExpr(type_expression env method_env e)
    | Block b -> TBlock(List.map type_statment b)
    | If(e,s,None) ->
        TIf(type_condition_expr e, type_statment s, None)
    | If(e,s1,Some s2) ->
        TIf(type_condition_expr e, type_statment s1, Some (type_statment s2))
    | While (e, s) ->
        TWhile(type_condition_expr e, type_statment s)
    | Return(Some e) -> type_return_stmt e
    | Return(None) -> type_none_return_stmt;
    | _ -> TNop (* a small cheat to avoid Match_failure *)
    (*TODO: check and type all the statments here *)

  and type_condition_expr e =
    let typed_e = type_expression env method_env e in
    let t = type_of_typed_expr typed_e in
    match t with
    | Primitive(Boolean) -> typed_e
    | _ -> raise(SyntaxError("condition expects a boolean type expression."))

  and type_return_stmt e =
    if not is_call then
      raise (Action_Not_Supported("return statement outside a method call"));
    let typed_e = type_expression env method_env e in
    let type_of_e = type_of_typed_expr typed_e in
    check_assignment_type env mreturntype type_of_e;
    TReturn(Some (typed_e))

  and type_none_return_stmt =
    if not is_call then
      raise (Action_Not_Supported("return statement outside a method call"));
    TReturn(None)

  in match l with
    | [] -> []
    | h::others -> (type_statment h)::(type_statement_list env method_env others is_call mreturntype)

let rec type_method_args_list env method_env l =
  let check_method_local_variable_redefined method_env id =
    if Env.mem method_env id then
      raise(Method_Local_Variable_Redefined("duplicated parameter: "^id))

  in let type_parameter env method_env pvd =
    begin
    check_type_ref_in_env pvd.ptype pvd.pident env;
    check_method_local_variable_redefined method_env pvd.pident;
    Env.add method_env pvd.pident pvd.ptype;
    {
      t_final = pvd.final;
      t_vararg = pvd.vararg;
      t_ptype = pvd.ptype;
      t_pident = pvd.pident;
    }
    end
  in match l with
    | [] -> []
    | h::others -> (type_parameter env method_env h)::(type_method_args_list env method_env others)


let rec type_attribute_list env l =
  let type_attr a =
    match a.adefault with
    | Some e ->
        begin
          (* Attention: the empty env below should not be modified *)
          let typed_e = type_expression env (Env.initial()) e in
          let t = type_of_typed_expr typed_e in
          let _ = check_assignment_type env a.atype t;
          in {
              t_amodifiers = a.amodifiers; t_aname = a.aname;
              t_atype = a.atype; t_adefault = Some (typed_e);
          }
        end
    | None ->
      {
        t_amodifiers = a.amodifiers; t_aname = a.aname;
        t_atype = a.atype; t_adefault = None;
      }

  in match l with
     | [] -> []
     | t::q -> (type_attr t)::(type_attribute_list env q)


let rec type_method_list env l =
  let typed_method m =
    (* method_env with key: variable_name, value: variable_type
     * it is used for local variable redifinition check.
     * *)
    let method_env = Env.initial() in
    let argstype = type_method_args_list env method_env m.margstype in
    {
      t_mmodifiers = m.mmodifiers;
      t_mname = m.mname;
      t_mreturntype = m.mreturntype;
      t_margstype = argstype;
      t_mbody = List.rev(type_statement_list env method_env (List.rev m.mbody) true m.mreturntype);
      t_mthrows = m.mthrows;
    }

  in match l with
     | [] -> []
     | t::q -> (typed_method t)::(type_method_list env q)


let type_initial_list env init_list =
  let l = [] in
  List.iter
  (fun initial ->
    l = List.append l
    [{
        t_static = initial.static;
        t_block = type_statement_list env (Env.initial()) initial.block false Void
    }]; ()
  ) init_list; l


(* constructors *)
let type_astconsts_list env astconsts_list =
  let l = [] in
  List.iter
  (fun a ->
    l = List.append l
    [{
      t_cmodifiers = a.cmodifiers;
      t_cname = a.cname;
      t_cargstype = type_method_args_list env (Env.initial()) a.cargstype;
      t_cthrows = a.cthrows;
      t_cbody = type_statement_list env (Env.initial()) a.cbody true Void;
    }]; ()
  ) astconsts_list; l



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
              t_cmethods = type_method_list env (List.rev c.cmethods); (* FIXME *)
              t_cattributes = type_attribute_list env c.cattributes;
              t_cparent = c.cparent;
              t_cinits = type_initial_list env c.cinits;
              t_cconsts = type_astconsts_list env c.cconsts;
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
