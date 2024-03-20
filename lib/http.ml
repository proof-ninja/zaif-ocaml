open Lwt
open Cohttp_lwt_unix
open Common

let curlcmd meth headers uri body =
  let headers =
    List.map (fun (k, v) -> !%"-H '%s: %s'" k v) headers |> String.concat " "
  in
  !%"curl -X %s %s --data '%s' '%s'" meth headers body (Uri.to_string uri)

let get ?(headers=[]) uri =
  Log.debug "Http.get $ %s" (curlcmd "GET" headers uri "");
  let headers = Cohttp.Header.of_list headers in
  Client.get ~headers uri >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body
  >>= fun body -> Log.debug "response: %s" body; Lwt.return body

let post ?(headers=[]) uri data =
  Log.debug "Http.post $ %s" (curlcmd "POST" headers uri data);
  let headers = Cohttp.Header.of_list headers in
  let body = Cohttp_lwt.Body.of_string data in
  Client.post ~headers uri ~body >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body
  >>= fun body -> Log.debug "response: %s" body; Lwt.return body
