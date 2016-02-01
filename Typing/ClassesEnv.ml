open Option
open Structure
open TypedStructure
open Errors

(**************************************************************************************************)
(******************** These functions build the classes definition environment ********************)

(* Basic types classes' environment. Object is the only one to have no parent. *)
let static_classes_env = 
	[
		{name="Int"; parent=Some "Object"; methods=[]; attributes=[]};
		{name="String"; parent=Some "Object"; methods=[]; attributes=[]};
		{name="Boolean"; parent=Some "Object"; methods=[]; attributes=[]};
		{name="Object"; parent=None; methods=[]; attributes=[]};
	]

(* This function checks that a type exists for the given classname, and returns it. *)
(* The classname is a non-located Structure.classname. *)
let rec type_of_classname currentClassesEnv r_type cn = 
	let cls_env = Env.find currentClassesEnv r_type
	in let cls_env.attributs
	match currentClassesEnv with 
	| [] -> raise (PError(UndefinedType(string_of_classname cn), loc))
	| t::q when t.name = (string_of_classname cn) -> t.name
	| t::q -> type_of_classname q cn loc

