(* An environment enables to store and access information associated
   with a string key. *)
type ('a,'b) t

(* creation of a new empty environment *)
val initial : unit -> ('a,'b) t

(* get the value associated to a key *)
val find : ('a,'b) t -> 'a -> 'b

(* is the key associated to a value is the environment *)
val mem : ('a,'b) t -> 'a -> bool

val copy : ('a, 'b) t -> ('a, 'b) t

(* define a key with the value associated *)
val define : ('a,'b) t -> 'a -> 'b -> ('a,'b) t

(* add a (key:value) pair to the environment *)
val add : ('a,'b) t -> 'a -> 'b -> unit

(* replace a (key:value) pair to the environment *)
val replace : ('a,'b) t -> 'a -> 'b -> unit

(* iterate a function over all the bindings of the environment *)
val iter : ('a * 'b -> unit) -> ('a,'b) t -> unit

(* get the hashtabl's keys by list*)
val hashtbl_keys : ('a, 'b) t -> 'a list

(* get the hashtabl's keys-sorted by list*)
val sort_hash_key : ('a, 'b) t -> 'a list
