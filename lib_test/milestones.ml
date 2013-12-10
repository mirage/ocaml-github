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
  let opro_milestones = Github.Milestone.for_repo ~user:"ocaml" ~repo:"opam" in
  Github.(Monad.run (opro_milestones ~state:`Closed ())) >|= print_milestones >>
  Github.(Monad.run (opro_milestones ~state:`Closed ~direction:`Asc ())) >|= print_milestones >>
  Github.(Monad.run (Milestone.for_repo ~sort:`Completeness ~direction:`Asc ~user:"mxcl" ~repo:"homebrew" ())) >|= print_milestones >>
  Github.(Monad.run (Milestone.for_repo ~sort:`Completeness ~direction:`Desc ~user:"mxcl" ~repo:"homebrew" ())) >|= print_milestones >>
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
