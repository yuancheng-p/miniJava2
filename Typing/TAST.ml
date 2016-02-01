open AST
open Structure
type typed_attr_or_method = 
  | TypedAttr of classname * string
  (*TODO| TypedAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string*)
  | TypedMethod of classname * string
  (*TODO| TypedStaticAttr of classname Located.t * string Located.t * string
  | TypedStaticAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string
  | TypedStaticMethod of classname Located.t * string Located.t * typed_param Located.t list 
  	* typed_expr Located.t * string*)

type typed_class_or_expr = 
  (*TODO| TypedClassdef of Type.ref_type * astattribute list * initial list * astconst list * astmethod list * asttype list * Location.t  *)
  | TypedClassdef of typed_attr_or_method list * typed_attr_or_method list


(* Retrieve a class definition from the classes environment, with its name. *)
(*TODO let rec get_classdef classesEnv classname_string = 
	let s c = 
		if (c.name = classname_string) then true else false
	in match classesEnv with
	| [] -> raise (PError(UndefinedType(classname_string)))
	| t::q -> if (s t) then t else get_classdef q classname_string*)



