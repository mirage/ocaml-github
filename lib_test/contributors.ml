open Printf
open Github_t

let token = Config.access_token
let user = "seveneng"
let repo = "ocaml-scry"

let t = Github.(Monad.(run (
  Repo.contributors ~token ~user ~repo ()
  >>~ fun contributors ->
  eprintf "login : total commits in [%s]/[%s]\n" user repo;
  List.iter (fun c ->
    eprintf "%s : %d\n"
      c.repo_contributor_author.user_login
      c.repo_contributor_total) contributors;
  return ()
)))

let () = Lwt_main.run t
