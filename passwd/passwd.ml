let get ~prompt =
  let open Lwt in
  let open Lwt_unix in
  tcgetattr stdin >>= fun term_io ->
  tcsetattr stdin TCSANOW { term_io with c_echo = false } >>= fun () ->
  Lwt_io.print prompt >>= fun () ->
  Lwt_io.read_line Lwt_io.stdin >>= fun input ->
  tcsetattr stdin TCSANOW term_io >|= fun () ->
  input

let get_if_unset ~prompt = function
  | "" -> get ~prompt
  | p -> Lwt.return p
