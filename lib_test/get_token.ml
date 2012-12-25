open Lwt
open Printf

let t =
  let r = Github.Token.create ~user:Config.user ~pass:Config.pass () in
  lwt auth = Github.Monad.run r in
  let token = Github.Token.of_auth auth in
  prerr_endline (Github.Token.to_string token);
  return ()

let _ = Lwt_main.run t
