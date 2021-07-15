open Printf

let token = Config.access_token

let print_pulls pl = Github.(Monad.(
  Stream.iter (fun p ->
    let open Github_t in
    eprintf "pull request %d: %s (%s)\n%!"
      p.pull_number p.pull_title p.pull_created_at;
    return ()
  ) pl
  >>= fun () ->
  eprintf "--\n%!";
  return ()
))
 
let t = Github.(Monad.(run (
  let user = "ocaml" in
  let repo = "opam" in
  let opam_repo_pulls = Pull.for_repo ~user ~repo in
  return (opam_repo_pulls ~state:`Open ()) >>= print_pulls >>= fun () ->
  return (opam_repo_pulls ~state:`Closed ()) >>= print_pulls >>= fun () ->
  return (opam_repo_pulls ())
  >>= Stream.iter (fun hd ->
    Pull.get ~token ~user ~repo ~num:hd.Github_t.pull_number ()
    >>~ fun p ->
    eprintf "Inside monad: pull %d: %s\n%!"
      p.Github_t.pull_number p.Github_t.pull_title;
    return (Pull.commits ~token ~user ~repo ~num:hd.Github_t.pull_number ())
    >>= Stream.iter (fun commit ->
      eprintf "    %s\n" commit.Github_t.commit_sha; return ()
    )
    >>= fun () ->
    eprintf "---------\n%!";
    return (Pull.files ~token ~user ~repo ~num:hd.Github_t.pull_number ())
    >>= Stream.iter (fun file ->
      eprintf "    %s\n" file.Github_t.file_filename; return ()
    )
  )
)))

;;

Lwt_main.run t
