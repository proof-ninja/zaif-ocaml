open Common

type t = float

let now () = Unix.gettimeofday ()

let to_millisec time = (time *. 1000.) |> int_of_float

(*"2015-07-07T08:45:53"*)
let from_string s =
  let dummy = 123 in
  Scanf.sscanf s "%04d-%02d-%02dT%02d:%02d:%02d" (fun year month tm_mday tm_hour tm_min tm_sec ->
      Unix.({
               tm_year=year-1900; tm_mon=month-1; tm_mday;
               tm_hour; tm_min; tm_sec;
               tm_wday=dummy; tm_yday=dummy; tm_isdst=false;
      })
      |> Unix.mktime
      |> fst)

let ymdhms time =
  let tm = Unix.localtime time in
  let year = tm.tm_year + 1900 in
  let month = tm.tm_mon + 1 in
  let day = tm.tm_mday in
  let hour = tm.tm_hour in
  let min = tm.tm_min in
  let sec = tm.tm_sec in
  !%"%04d_%02d_%02d-%02d_%02d_%02d"
    year month day hour min sec
