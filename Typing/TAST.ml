open AST
open Structure
open Errors

type typed_attr_or_method = 
  | TypedAttr of classname * string
  (*TODO| TypedAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string*)
  | TypedMethod of classname * string * typed_param list * string
  | TypedStaticMethod of classname * string * typed_param list * string
  (*TODO| TypedStaticAttr of classname Located.t * string Located.t * string
  | TypedStaticAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string
  | TypedStaticMethod of classname Located.t * string Located.t * typed_param Located.t list 
  	* typed_expr Located.t * string*)

type typed_class_or_expr = 
  (*TODO| TypedClassdef of Type.ref_type * astattribute list * initial list * astconst list * astmethod list * asttype list * Location.t  *)
  | TypedClassdef of typed_attr_or_method list * typed_attr_or_method list


(* Retrieve a class definition from the classes environment, with its name. *)
let get_classdef classes_env ref_type =
        (*Env.find classes_env ref_type*)
        let clfind = Env.mem classes_env ref_type in
        if clfind then Env.find classes_env ref_type
        (*else print_endline "123" *)
        (*| false -> raise(PError(UndefinedType(ref_type.id)))*)



