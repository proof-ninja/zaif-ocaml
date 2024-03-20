open Common

(* HTTP Public APIs *)

let host = "api.zaif.jp"
let end_point = "/tapi"

let make_nonce () =
  let now = Datetime.now() in
  let base_time = Datetime.from_string "2024-03-20T01:01:01" in
  (now -. base_time)
  |> int_of_float
  |> string_of_int

let post auth command params =
  let nonce = make_nonce () in
  let query =
    ("method", command)
    :: ("nonce", nonce)
    :: params
  in
  let postdata =
    list_group_by fst query
    |> List.map (fun (k,v) -> (k, List.map snd v))
    |> Uri.encoded_of_query
  in
  let uri =
    Uri.make ~scheme:"https" ~host ~path:end_point ()
  in
  let headers = Auth.make_header auth postdata in
  Http.post ~headers uri postdata

let get_info auth =
  post auth "get_info" []

type order = {
    order_id : int;
    currency_pair : string;
    action : side;
    amount: float;
    price : int;
    timestamp: Datetime.t;
    comment: string;
}

let active_orders ?currency_pair auth =
  let params =
    match currency_pair with
    | Some cpair -> [("currency_pair", cpair)]
    | None -> []
  in
  post auth "active_orders" params

(*指値注文のみ可能*)
let trade auth currency_pair side price amount =
  let params = [
      ("currency_pair", currency_pair);
      ("action", string_of_side side);
      ("price", string_of_int price);
      ("amount", string_of_float amount);
    ]
  in
  post auth "trade" params
