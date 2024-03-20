open Zaif

let auth = Auth.auth ()

let () =
  Log.set_log_level Log.DEBUG;
  Log.debug "Hello, Zaif";
  Lwt_main.run begin
      Zaif.Private.get_info auth
      |> Lwt.map (fun _json -> ())
    end
