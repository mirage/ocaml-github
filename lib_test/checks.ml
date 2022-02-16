open Cmdliner

let token = Config.access_token

let list_check_runs_for_ref owner repo sha () =
  Lwt_main.run begin
    let open Github in
    let open Monad in
    run (
      Check.list_check_runs_for_ref ~token ~owner ~repo ~sha ()
      >>~ fun check ->
      (List.iter (fun x -> Printf.printf "check_run_name %s check_run_id %Ld\n" x.Github_j.check_run_name x.Github_j.check_run_id) check.Github_j.check_runs);
      return ()
    )
  end

let get_check_run owner repo check_run_id () =
  Lwt_main.run begin
    let open Github in
    let open Monad in
    run (
      Check.get_check_run ~token ~owner ~repo ~check_run_id ()
      >>~ fun check_run ->
      Printf.printf "check_run_name %s check_run_id %s\n" check_run.Github_j.check_run_name (Github_j.string_of_check_status check_run.Github_j.check_run_status);
      return ()
    )
  end

module CommandLine = struct
  let repo =
    let doc = "Repository" in
    Arg.(required
         & opt (some string) None
         & info ["r"; "repository"] ~docv:"REPOSITORY" ~doc)

  let owner =
    let doc = "Owner" in
    Arg.(required
         & opt (some string) None
         & info ["o"; "owner"] ~docv:"OWNER" ~doc)

  let sha =
    let doc = "Git SHA" in
    Arg.(required
         & opt (some string) None
         & info ["s"; "sha"] ~docv:"GIT_SHA" ~doc)

  let check_run_id =
    let doc = "Check Run Id" in
    Arg.(required
         & opt (some string) None
         & info ["c"; "check_run_id"] ~docv:"CHECK_RUN_ID" ~doc)

  let list_cmd =
    let term = Term.(const list_check_runs_for_ref $ owner $ repo $ sha $ const ()) in
    let info = Cmd.info "list" ~doc:"List Check Runs for a git sha" in
    Cmd.v info term

  let get_check_run =
    let term = Term.(const get_check_run $ owner $ repo $ check_run_id $ const ()) in
    let info = Cmd.info "get-check" ~doc:"Get a Check Run for check run id" in
    Cmd.v info term

  let cmds =
    let default = Term.(ret (const (`Help (`Pager, None)))) in
    let default_info = Cmd.info "checks" ~version:"0.1" ~doc:"Github Checks API." in
    Cmd.group ~default default_info [get_check_run; list_cmd]
end

let cmdliner =
  exit @@ Cmd.eval CommandLine.cmds
