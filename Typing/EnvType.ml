open AST
open Type

type class_env = {
  parent: ref_type;
  methods: (t_method_signiture, method_env) Env.t;
  attributes: string list; (* TODO replace me by attributes list *)
}
and method_env = {
  mutable modifiers: modifier list;
  return_type: t;
  (* TODO throws *)
}
and t_method_signiture = {
  name: string;
  args: t_arg list;
}
and t_arg = {
  tvararg: bool;
  tptype: t;
}


let rec mk_t_args arguments l =
  match arguments with
  | [] -> l
  | h::others ->
      mk_t_args others ({tvararg=h.vararg;tptype=h.ptype}::l)
