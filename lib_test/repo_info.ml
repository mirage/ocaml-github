open Lwt
open Printf
open Github_t

let token = Config.access_token

let t =
  lwt r = 
    let open Github.Monad in
    run (
    Github.Repo.info ~token ~user:"ocaml" ~repo:"opam" () >>=
    fun info ->
      let descr = match info.repository_description with
      | Some descr -> descr
      | None -> ""
      in
      eprintf "repo %s\n" descr;
      Github.Repo.branches ~token ~user:"ocaml" ~repo:"opam" () >>=
      fun branches ->
        List.iter (fun b ->
        eprintf "branch %s %s\n"
          b.repo_branch_name
          b.repo_branch_commit.repo_commit_sha
        ) branches;
        return ()
  ) in
  return ()

let _ = Lwt_main.run t
