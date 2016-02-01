open Structure
open TypedStructure
open Errors

(**************************************************************************************************)
(******************** These functions build the classes definition environment ********************)

(* This function checks that a type exists for the given classname, and returns it. *)
(* The classname is a non-located Structure.classname. *)
let rec type_of_paraname currentClassesEnv pn = 
	match currentClassesEnv with 
	| [] -> raise (PError(UndefinedType(string_of_classname cn), loc))
	| t::q when t.name = (string_of_classname cn) -> t.name
	| t::q -> type_of_classname q cn
