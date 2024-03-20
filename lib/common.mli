val (!%) :  ('a, unit, string) format -> 'a

val list_take : int -> 'a list -> 'a list

val list_group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list

type side = Buy | Sell
val side_of_string : string -> side
val string_of_side : side -> string

module Log = Dolog.Log
module Json = Yojson.Basic
