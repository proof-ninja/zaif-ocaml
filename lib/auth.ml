open Common

let default_filename = "zaif-auth.conf"

type t = {
    api_key : string;
    secret : string;
}

let from_file ?(filename=default_filename) () =
  let ch = open_in filename in
  let api_key = input_line ch in
  let secret = input_line ch in
  close_in ch;
  {api_key; secret}

let auth () = from_file ()

let sign auth postdata =
  let text = postdata in
  let secret = auth.secret in
  Hacl_star.EverCrypt.HMAC.mac ~alg:SHA2_512
    ~key:(Bytes.of_string secret) ~msg:(Bytes.of_string text)
  |> Hex.of_bytes
  |> Hex.show
  |> fun s -> Log.debug "Auth.sign: '%s' ==> '%s'" text s; s


let make_header auth queryparams =
  let s = sign auth queryparams in
    [
      ("Key", auth.api_key);
      ("Sign", s);
(*      ("Content-Type", "application/json");*)
    ]
