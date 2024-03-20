val get : ?headers:(string * string) list -> Uri.t -> string Lwt.t

val post : ?headers:(string * string) list -> Uri.t -> string -> string Lwt.t
