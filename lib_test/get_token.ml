open Lwt
open Printf

let t = Github.(
  let r = Token.create ~user:Config.user ~pass:Config.pass ~note:"get_token via ocaml-github" () in
  Monad.run r
  >>= function
  | Result auth ->
    let token = Token.of_auth auth in
    prerr_endline (Token.to_string token);
    return ()
  | Auth (_,_) -> fail_with "get_token doesn't support 2fa, yet"
)

;;

Lwt_main.run t
