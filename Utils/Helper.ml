(* type option -> type *)
let trim_option_type t default =
  match t with
  | Some p -> p
  | None -> default
