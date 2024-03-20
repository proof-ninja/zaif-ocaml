open Lwt
open Bitflyer
module C = Bitflyer__Common

let (!%) s = Printf.sprintf s

(*slack用のログは標準出力に出す*)
let p s = Printf.printf s

(*詳細のデバッグログはLogでローカルファイル出力する*)
let setup_log () =
  Log.(set_log_level DEBUG);
  let logfile = !%"bitflyer-trarep_%s.log" (Datetime.(ymdhms (now()))) in
  let ch = open_out logfile in
  Log.set_output ch;
  ch

let coin = "ETH_JPY"

(*let start_date = Datetime.from_string "2024-03-11T06:00:00"*)
let start_date = Datetime.from_string "2024-03-11T09:00:00"

type yen = Yen of int

type coin = Coin of float
(*注文のときは0.0000001 単位じゃないとできない*)
let size_of_coin (Coin amount) =
  float_of_string (!%"%0.07f" amount)


let auth = Auth.auth ()

let limit (Yen price) = (*5000で丸める*)
  Yen (5000 * (price / 5000))

let lot = Yen 10000 (*一回の買い注文で買う量[円]*)

let target (Yen price) = (*買いに対応する売りの目標*)
  Yen (price + 5000)

let amount (Yen price) =
  let (Yen lot) = lot in
  Coin (float lot /. float price)

open Bitflyer__Trade

let is_market o = match o.child_order_type with
  | C.Market -> true | Limit _ -> false

let price_of order = match order.child_order_type with
  | Limit p -> p

let sorder_type = function
| C.Market -> "Market"
| Limit p -> !%"Limit %d" (int_of_float p)

let sorder (o:order) =
  !%"%s %f(%s) %s" (C.string_of_side o.side) o.size
    (sorder_type o.child_order_type) (Datetime.ymdhms o.child_order_date)

let sorders os = List.map sorder os |> String.concat ", "

let eq_order_type x y =
  match (x, y) with
  | (C.Market, C.Market) -> true
  | (Limit x, Limit y) when abs_float (x -. y) < 0.01 -> true
  | _ -> false

let is_recently date =
  let now_ = Datetime.now () in
  let d = 30 * 60 * 1000 (* 30 min *) in
  Datetime.(to_millisec now_ - d < to_millisec date)

let is_target_price_of buy sell =
  match buy.child_order_type with
  | Market -> false
  | Limit p ->
     let Yen target_price = target (Yen (int_of_float p)) in
     eq_order_type sell.child_order_type (Limit (float target_price))

let buy_positions completed_orders = (*買いポジションを計算*)
  let rec remove_buy_position sell = function
    | [] ->
       p "sellに対応する買いポジションがないぞ???\n";
       []
    | buy :: rest when is_target_price_of buy sell ->
       rest
    | buy :: rest -> buy :: remove_buy_position sell rest
  in
  completed_orders
  |> List.filter(fun o -> o.child_order_date > start_date)
  |> List.rev
  |> List.fold_left (fun positions (order: order) ->
         match order.side with
         | Buy ->
            order :: positions
         | Sell ->
            remove_buy_position order positions) []


let work () =
  (*現在値をとる*)
  BitflyerApi.getticker coin >>= fun ticker ->
  let Yen price = (*現在値*)
(*    ((ticker.best_ask +. ticker.best_bid) /. 2.)*)
    ticker.best_bid (*低い方*)
    |> fun x -> Yen (int_of_float x)
  in
  let Yen closest_limit = limit (Yen price) in
  p "Ticker %s %s: Bid:%f, Ask:%f, price=%d, target=%d\n" coin ticker.timestamp ticker.best_bid ticker.best_ask price closest_limit;
  (*自分の注文一覧(未約定)をとる*)
  BitflyerApi.getchildorders auth coin >>= fun orders ->
  let active_orders = List.filter (fun o -> o.child_order_state = "ACTIVE") orders in
  let () =
    p "My orders: [%s]\n"
      (C.list_take 10 active_orders |> List.map sorder |> String.concat ", ");
  in
  (*約定一覧を取る*)
  let completed_orders =
    List.filter (fun o -> o.child_order_state = "COMPLETED") orders
  in
  let buys = buy_positions completed_orders in (*売買履歴から買いポジションをリストに*)
  let () =
    p "My completed orders: [%s]\n"
      (C.list_take 10 completed_orders|> List.map sorder |> String.concat ", ")
  in
  let () =
    p "買いポジ: [%s]\n" (List.map sorder buys |> String.concat ", ")
  in
  (*もし現在値の直下の注文がなければ買い注文をいれる (最近上昇したはず)*)
  let find os = List.find_opt (fun (order: order) ->
                    eq_order_type order.child_order_type
                      (Limit (float closest_limit))
                    && order.side = Buy) os
                |> function
                  | None -> p "find[%d]: なかった\n" closest_limit; None
                  | Some o -> p "find[%d]: あった! [%s]\n" closest_limit (sorder o); Some o
  in
  begin match find active_orders, find buys with
  | None, None ->
     p "なかったから買うんだぞ\n";
     let amount = amount (Yen closest_limit) in
     let size = size_of_coin amount in
     BitflyerApi.sendchildorder auth coin (Limit (float closest_limit)) Buy size
     >>= fun json ->
     p "買注文だん %f: %s\n" size (C.Json.to_string json);
     Lwt.return ()
  | _, _ ->
     p "買いません: %d\n" closest_limit;
     Lwt.return () (*すでに買い注文があれば何もしない*)
  end >>= fun () ->
  (*もし買ポジションに対する売り注文がない場合は売り注文をいれる (最近約定したはず) *)
(*  let baughts =
    List.filter (fun (o:order) -> o.side = Buy) completed_orders
    |> List.filter (fun o -> is_market o = false) (*指値じゃない注文は無視する*)
  in*)
  buys
  |> Lwt_list.iter_s (fun buy_pos ->
         let is_target_order_exists target_price =
           active_orders
           |> List.exists (fun (active:order) ->
                  eq_order_type active.child_order_type (Limit (float target_price))
                  && active.side = Sell)
         in
         let baught_price = price_of buy_pos in
         let Yen target_price = target (Yen (int_of_float baught_price)) in
         if is_target_order_exists target_price then
           let _ = p "売りません: 売り注文は存在します: %d\n" target_price in
           Lwt.return ()
         else if target_price < price - 1000 then
           let _ = p "売りません: それにしても目標値が安すぎるので: %d\n" target_price in
           Lwt.return ()
         else
           let size = size_of_coin (amount (Yen target_price)) in
           let _ = p "売り注文やで %d %f\n" target_price size in
           BitflyerApi.sendchildorder auth coin (Limit (float target_price)) Sell size
           >>= fun json ->
           p "売注文だん %d %f: %s\n" target_price size (C.Json.to_string json);


           Lwt.return ()) >>= fun () ->
  Lwt.return ()

let () =
  p "Hello, Trarep start_date: %s\n" (Datetime.ymdhms start_date);
  let ch = setup_log () in
  Lwt_main.run (work () >>= fun () ->
    Lwt.return (close_out ch));
