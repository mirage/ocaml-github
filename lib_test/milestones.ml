open Lwt
open Printf

let token = Config.access_token

let print_milestones m =
  List.iter (fun m ->
    let open Github_t in
    eprintf "milestone %d: %s (%s)\n%!" m.milestone_number m.milestone_title m.milestone_created_at
  ) m;
  eprintf "--\n%!"
 
let t =
  lwt m1 = Github.(Monad.run (Milestone.for_repo ~state:`Closed ~token ~user:"OCamlPro" ~repo:"opam" ())) in
  print_milestones m1;
  lwt m1 = Github.(Monad.run (Milestone.for_repo ~state:`Closed ~direction:`Asc ~token ~user:"OCamlPro" ~repo:"opam" ())) in
  print_milestones m1;
  lwt m2 = Github.(Monad.run (Milestone.for_repo ~sort:`Completeness ~direction:`Asc ~token ~user:"mxcl" ~repo:"homebrew" ())) in
  print_milestones m2;
  lwt m3 = Github.(Monad.run (Milestone.for_repo ~sort:`Completeness ~direction:`Desc ~token ~user:"mxcl" ~repo:"homebrew" ())) in
  print_milestones m2;
  let user = "mxcl" in
  let repo = "homebrew" in
  Github.(Monad.(run (
    Milestone.for_repo ~sort:`Completeness ~direction:`Desc ~token ~user ~repo ()
    >>= fun milestones ->
      let rec iterate =
        function
        |[] -> return ()
        |hd::tl ->
           Milestone.get ~token ~user ~repo ~num:hd.Github_t.milestone_number ()
           >>= fun m ->
            eprintf "Inside monad: milestone %d: %s\n" m.Github_t.milestone_number m.Github_t.milestone_title;
            iterate tl
      in 
      iterate milestones
  )))

let _ = Lwt_main.run t
