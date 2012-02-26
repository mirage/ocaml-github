open Lwt
open Printf

let token = Config.access_token

let t =
  lwt r = 
    let open Github.Monad in
    run (
    Github.Issues.for_repo ~token ~user:"avsm" ~repo:"mirage" () >>=
    fun issues ->
      List.iter (fun issue ->
        let open Github.Issues in
        eprintf "issue %d: %s\n%!" issue.number issue.title
      ) issues;
      return ()
  ) in
  return ()

let _ = Lwt_main.run t
