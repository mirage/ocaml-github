val get : prompt:string -> string Lwt.t

val get_if_unset : prompt:string -> string option -> string Lwt.t
