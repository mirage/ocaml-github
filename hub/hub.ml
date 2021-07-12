(** See https://hub.github.com/hub.1.html *)
open Cmdliner
open Printf

type configuration = {
    token: Github.Token.t;
    user: string
  }

exception Config of string

(** Read hub configuration from ~/.config/hub. *)
let config_from_file () =
  let home = Unix.getenv "HOME" in
  let fpath = home ^ "/.config/hub" in
  let result = Yaml_unix.of_file Fpath.(v fpath) in
  let config_err = Config "Expected ~/.config/hub to be readable and exist as created by man hub(1)."  in
  match result with
  | Ok (`O [("github.com", `A [`O [("user", `String user_str);
                                   ("oauth_token", `String token_str);
                                   ("protocol", _)]])]) ->
     { token = (Github.Token.of_string token_str)
     ; user = user_str }
  | Error _ | Ok _ -> raise config_err

let release token user repo () =
  let print_releases pl =
    let open Github in
    let open Monad in
    Stream.iter (fun m ->
        printf "%s\n" (Github_j.string_of_release m);
        return ()
      ) pl
  in
  Lwt_main.run begin
      let open Github in
      let open Monad in
      run (
          return (Release.for_repo ~token ~user ~repo ())
          >>= print_releases
        )
  end

let release_show token user repo tag show_downloads () =
  let open Github in
  let open Monad in
  Lwt_main.run begin
      run (
          Release.get_by_tag_name ~token ~user ~repo ~tag ()
          >>~ fun release -> 
              printf "\n\n%s\n" (Option.value ~default:"" release.Github_j.release_body);
              if show_downloads then List.iter (fun r -> printf "### Downloads\n\n%s\n" r.Github_j.release_asset_browser_download_url) release.Github_j.release_assets;
              printf "%s\n%s\n"release.Github_j.release_tarball_url release.Github_j.release_zipball_url;
              return ()
        )
    end

let pr_list token user repo state () =
  let open Github in
  let open Monad in

  let print_pulls pl =
    Stream.iter (fun m -> printf "    #%d  %s\n" m.Github_t.pull_number m.Github_t.pull_title;
                          return ()) pl
  in
  Lwt_main.run begin
      run (
          return (Pull.for_repo ~token ~user ~repo ?state:(Some state) ())
          >>= print_pulls
        )
    end  

(** CommandLine options parsing module. 
    Reusable pieces between different commands.
*)
module CommandLine = struct
  let repo =
    let doc = "Github Repository" in
    Arg.(required
         & opt (some string) None
         & info ["r"; "repository"] ~docv:"REPOSITORY" ~doc)

  let owner =
    let doc = "Github Owner" in
    Arg.(required
         & opt (some string) None
         & info ["o"; "owner"] ~docv:"OWNER" ~doc)

  let release_name =
    let doc = "Release Tag" in
    Arg.(required
         & opt (some string) None
         & info ["t"; "tag"] ~docv:"TAG" ~doc)

  let show_downlods =
    Arg.(value
         & flag
         & info ["show-downloads"] ~doc:"With --show-downloads, include the \"Downloads\" section.")

  let state =
    let doc = "Pull request state. Either open, closed, or all to filter by state." in
    let states = 
      [("open", `Open); ("closed", `Closed); ("all", `All)] in

    Arg.(value @@ opt (enum states) `Open @@ 
         info ["s"] ~docv:"STATE" ~doc)
end

(* Executable commands from the cli. *)
let release_cmd =
  let config = config_from_file () in
  Term.(pure release $ pure config.token $ CommandLine.owner $ CommandLine.repo $ pure ()),
  Term.info "release" ~doc:"Manage GitHub Releases for the current repository."

let release_show_cmd =
  let config = config_from_file () in
  Term.(pure release_show $ pure config.token $ CommandLine.owner $ CommandLine.repo $ CommandLine.release_name $ CommandLine.show_downlods $ pure ()),
  Term.info "release-show" ~doc:"Show GitHub release notes for TAG."

let pr_list_cmd =
  let config = config_from_file () in
  Term.(pure pr_list $ (pure config.token) $ CommandLine.owner $ CommandLine.repo $ CommandLine.state $ pure ()),
  Term.info "pr-list" ~doc:"List pull requests in the current repository."

let default_cmd =
  let doc = "make git easier with GitHub" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
      `S "DESCRIPTION";
      `P "Hub is a tool that wraps git in order to extend it with extra functionality that makes it better when working with GitHub.";
      `S "BUGS";
      `P "<https://github.com/mirage/ocaml-github/issues>";
      `S "AUTHORS";
      `P "<https://github.com/mirage/ocaml-github>"
    ] in
  Term.info "hub" ~version:"0.1" ~doc ~man

(* All supported commands. *)
let cmds = [ release_cmd
           ; release_show_cmd
           ; pr_list_cmd ]

let () =
  try
    match Term.eval_choice ~catch:false default_cmd cmds with
    | `Error _ -> exit 1 | _ -> exit 0
  with
  | Github.Message (_,m) ->
     Printf.eprintf "GitHub API error: %s\n" (Github.API.string_of_message m);
     exit 1
