let (!%) s = Printf.sprintf s

let list_take n xs =
  List.to_seq xs |> Seq.take n |> List.of_seq

let list_upsert_assoc key value dict =
  (key, value) :: List.remove_assoc key dict

let list_group_by f xs =
  List.fold_left (fun dict x ->
      let key = f x in
      match List.assoc_opt key dict with
      | None -> (key, [x]) :: dict
      | Some ys -> list_upsert_assoc key (x :: ys) dict) [] xs

type side = Buy | Sell

let side_of_string = function
  | "bid" -> Buy
  | "ask" -> Sell
  | other -> failwith (!%"Common.side_of_string: '%s'" other)

let string_of_side = function
  | Buy -> "bid"
  | Sell -> "ask"

module Log = Dolog.Log

module Json = Yojson.Basic
