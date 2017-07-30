let t = Github.(Monad.(run (
  let note = "get_token via ocaml-github" in
  Token.create ~user:Config.user ~pass:Config.pass ~note ()
  >>~ function
  | Result auth ->
    let token = Token.of_auth auth in
    prerr_endline (Token.to_string token);
    return ()
  | Two_factor _ -> fail (Failure "get_token doesn't support 2fa, yet")
)))

;;

Lwt_main.run t
