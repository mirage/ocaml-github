open Lwt
open Printf

let token = Config.access_token

let print_milestones m =
  List.iter (fun m ->
    let open Github_t in
    eprintf "milestone %d: %s (%s)\n%!" m.milestone_number m.milestone_title m.milestone_created_at
  ) m;
  eprintf "--\n%!"
 
let t = Github.(Monad.(run (
  let opro_milestones = Milestone.for_repo ~user:"ocaml" ~repo:"opam" in
  let milestones = opro_milestones ~state:`Closed () in
  Stream.to_list milestones
  >|= print_milestones >>= fun () ->
  let milestones = opro_milestones ~state:`Closed ~direction:`Asc () in
  Stream.to_list milestones
  >|= print_milestones >>= fun () ->
  let milestones = Milestone.for_repo
      ~sort:`Completeness ~direction:`Asc ~user:"mxcl" ~repo:"homebrew" ()
  in Stream.to_list milestones
  >|= print_milestones >>= fun () ->
  let milestones = Milestone.for_repo
      ~sort:`Completeness ~direction:`Desc ~user:"mxcl" ~repo:"homebrew" ()
  in Stream.to_list milestones
  >|= print_milestones >>= fun () ->
  let user = "mxcl" in
  let repo = "homebrew" in
  API.set_token token >>= fun () ->
  let milestones = Milestone.for_repo
      ~sort:`Completeness ~direction:`Desc ~user ~repo ()
  in Stream.iter (fun { Github_t.milestone_number = num } ->
    Milestone.get ~user ~repo ~num ()
    >>= fun { Github_t.milestone_title } ->
    eprintf "Inside monad: milestone %d: %s\n" num milestone_title;
    return ()
  ) milestones
)))

;;

Lwt_main.run t
