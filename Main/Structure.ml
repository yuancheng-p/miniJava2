(* Every node of a data structure contains located elements. 
This will allow better error handling once the parsing phase is over. *)



type classname =
    (* Name of an already defined class *)
  | Classname of string

