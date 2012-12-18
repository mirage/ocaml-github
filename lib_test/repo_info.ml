open Lwt
open Printf
open Github_t

let token = Config.access_token

let t =
  lwt r = 
    let open Github.Monad in
    run (
    Github.Repo.info ~token ~user:"OCamlPro" ~repo:"opam" () >>=
    fun info ->
      eprintf "repo %s\n" info.repo_description;
      return ()
  ) in
  return ()

let _ = Lwt_main.run t
