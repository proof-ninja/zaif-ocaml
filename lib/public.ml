open Lwt
open Common

(* HTTP Public APIs *)

let host = "api.zaif.jp"
let end_point = "/api/1"

let get pathname =
  let uri =
    Uri.make ~scheme:"https" ~host ~path:pathname ()
    |> fun uri -> Uri.with_query' uri []
  in
  Unix.sleep 1;
  Http.get uri |> Lwt.map Json.from_string

let currencies currency =
  let path = end_point ^ (!%"/currencies/%s" currency) in
  get path

type ticker = {
    last: float;
    high: float;
    low: float;
    vwap: float;
    volume: float;
    bid: float;
    ask: float;
}

let ticker_of_json json =
  let open Json.Util in
  Log.debug "%s" (Json.show json);
  let last = member "last" json |> to_float in
  let high = member "high" json |> to_float in
  let low = member "low" json |> to_float in
  let vwap = member "vwap" json |> to_float in
  let volume = member "volume" json |> to_float in
  let bid = member "bid" json |> to_float in
  let ask = member "ask" json |> to_float in
  {
    last; high; low; vwap; volume; bid; ask;
  }

let ticker currency_pair =
  let path = !%"%s/ticker/%s" end_point currency_pair in
  get path >>= fun json ->
  Lwt.return (ticker_of_json json)

let depth currency_pair =
  let path = !%"%s/depth/%s" end_point currency_pair in
  get path >>= fun json ->
  Lwt.return json
