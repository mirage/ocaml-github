open Lwt
open Printf
open Github_t

let token = Config.access_token

let ask_github fn = Github.(Monad.run (fn ()))

let t =
  let get_teams = Github.Organization.teams ~token:token ~org:"hammerlab" in
  ask_github get_teams >>= fun teams ->
  let first_team = List.hd teams in
  let get_first_team = Github.Team.info ~token:token ~id:first_team.team_id in
  ask_github get_first_team >>= fun team ->
  eprintf "team %d: %s (%s)\n%!"
    team.team_info_id team.team_info_name team.team_info_url;
  let get_team_repos =
    Github.Team.repositories ~token:token ~id:team.team_info_id
  in
  ask_github get_team_repos >>= fun repos ->
  let first_repo = List.hd repos in
  eprintf "repo %d: %s\n%!" first_repo.repository_id first_repo.repository_name;
  return ()

let _ = Lwt_main.run t
