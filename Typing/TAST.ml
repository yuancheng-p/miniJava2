type classname =
    (* Name of an already defined class *)
  | Classname of string


type typed_attr_or_method =
  | TypedAttr of classname * string
  (*TODO| TypedAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string*)

  | TypedMethod of classname * string * string list * Type.t
  | TypedStaticMethod of classname * string * string list * Type.t

  (*TODO| TypedStaticAttr of classname Located.t * string Located.t * string
  | TypedStaticAttrWithValue of classname Located.t * string Located.t * typed_expr Located.t * string
  | TypedStaticMethod of classname Located.t * string Located.t * typed_param Located.t list
  	* typed_expr Located.t * string
  *)


type typed_class_or_expr =
  (*TODO| TypedClassdef of Type.ref_type * astattribute list * initial list * astconst list * astmethod list * asttype list * Location.t  *)
  | TypedClassdef of typed_attr_or_method list * typed_attr_or_method list

