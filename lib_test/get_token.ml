open Lwt
open Printf

let t =
  let r = Github.Token.direct ~user:Config.user ~pass:Config.pass () in
  lwt token = Github.Monad.run r in
  prerr_endline (Github.Token.to_string token);
  return ()

let _ = Lwt_main.run t
