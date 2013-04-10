open Lwt
open Printf

let token = None (* Some Config.access_token *)

let print_pulls pl =
  List.iter (fun p ->
    let open Github_t in
    eprintf "pull request %d: %s (%s)\n%!" p.pull_number p.pull_title p.pull_created_at
  ) pl;
  eprintf "--\n%!"
 
let t =
  let user = "dsheets" in
  let repo = "opam-repository" in
  let opam_repo_pulls = Github.Pull.for_repo ~user ~repo in
  Github.(Monad.run (opam_repo_pulls ~state:`Open ())) >|= print_pulls >>
  Github.(Monad.run (opam_repo_pulls ~state:`Closed ())) >|= print_pulls >>
  Github.(Monad.(run (
    opam_repo_pulls ()
    >>= fun pulls ->
    let rec iterate = function
      | [] -> return ()
      | hd::tl ->
          Pull.get ?token ~user ~repo ~num:hd.Github_t.pull_number ()
          >>= fun p ->
          eprintf "Inside monad: pull %d: %s\n" p.Github_t.pull_number p.Github_t.pull_title;
          Pull.list_commits ?token ~user ~repo ~num:hd.Github_t.pull_number ()
          >>= fun commits -> List.iter (fun commit ->
            eprintf "    %s\n" commit.Github_t.commit_sha
          ) commits;
          eprintf "---------\n";
          Pull.list_files ?token ~user ~repo ~num:hd.Github_t.pull_number ()
          >>= fun files -> List.iter (fun file ->
            eprintf "    %s\n" file.Github_t.file_filename
          ) files;
          iterate tl
    in
    iterate pulls
  )))

let _ = Lwt_main.run t
