open Lwt
open Zaif

let auth = Auth.auth ()

let currency_pair = "btc_jpy"
let () =
  Log.set_log_level Log.DEBUG;
  Log.debug "Hello, Zaif";
  try
    Lwt_main.run begin
      Zaif.Private.get_info auth >>= fun _json ->
      Lwt_unix.sleep 0.1 >>= fun () ->
(*      Zaif.Private.trade auth currency_pair Buy 920_0000 0.001 >>= fun _ ->
      Lwt_unix.sleep 0.1 >>= fun () ->*)
      Zaif.Private.active_orders ~currency_pair auth >>= fun _ ->
      Zaif.Public.depth currency_pair >>= fun json ->
      return ()
    end
  with
  | e -> prerr_endline (Printexc.to_string e)
