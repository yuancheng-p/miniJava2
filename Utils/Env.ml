type ('a,'b) t = ('a, 'b) Hashtbl.t

let initial () = (Hashtbl.create 17 : ('a,'b) t)

let find env = Hashtbl.find env

let mem env = Hashtbl.mem env

let copy env = Hashtbl.copy env

let define env n t =
  let result = Hashtbl.copy env in
    Hashtbl.add result n t;
    result

let add env n t = Hashtbl.add env n t

let replace env n t = Hashtbl.replace env n t

let iter f = Hashtbl.iter (fun s i -> f (s,i))

let hashtbl_keys h = Hashtbl.fold (fun key _ l -> key :: l) h []

let sort_hash_key hstab = 
  List.sort compare (hashtbl_keys hstab)
