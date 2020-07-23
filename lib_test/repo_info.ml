open Printf
open Github_t

let token = Config.access_token

let opam_first_commit = "3656b4d1b03a8ae356cf82f7052f1976df8787be"

let t =
  let open Github in
  let open Monad in
  run (
    Repo.info ~token ~user:"ocaml" ~repo:"opam" ()
    >>~ fun info ->
    let descr = match info.repository_description with
      | Some descr -> descr
      | None -> ""
    in
    eprintf "repo %s\n" descr;
    begin match info.repository_permissions with
      | Some permissions ->
        eprintf "permissions admin(%B) push(%B) pull(%B)\n"
          permissions.repository_permissions_admin
          permissions.repository_permissions_push
          permissions.repository_permissions_pull
      | None -> ()
    end;
    let branches = Repo.branches ~token ~user:"ocaml" ~repo:"opam" () in
    Stream.to_list branches
    >>= fun branches ->
    List.iter (fun b ->
      eprintf "branch %s %s\n"
        b.repo_branch_name
        b.repo_branch_commit.repo_commit_sha
      ) branches;
    Repo.get_commit ~token ~user:"ocaml" ~repo:"opam" ~sha:opam_first_commit ()
    >>~ fun commit ->
    eprintf
      "opam first commit author date: %s\n"
      commit.commit_git.git_commit_author.info_date;
    eprintf
      "opam first commit committer date: %s\n"
      commit.commit_git.git_commit_author.info_date;
    return ()
  )

;;
Lwt_main.run t
