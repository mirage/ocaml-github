open Printf
open Github_t

let user = "ocaml"
let repo = "opam-repository"

let get_auth_token_from_jar auth_id = Lwt.(
  Github_cookie_jar.init () >>= fun jar ->
  Github_cookie_jar.(get jar auth_id) >>= function
  | Some x -> return x
  | None -> Lwt.fail (Failure ("id '"^auth_id^"' not in cookie jar"))
)

let last_seen =
  List.fold_left
    (fun last { Github_t.repo_contribution_week_w = w;
                repo_contribution_week_a = a;
                repo_contribution_week_d = d;
                repo_contribution_week_c = c;
              } ->
      let active = a <> 0 || d <> 0 || c <> 0 in
      match last with
      | None -> if active then Some w else last
      | Some last_week when w > last_week -> if active then Some w else last
      | Some _ -> last
    ) None

let month_of_time_opt = function
  | None -> "never"
  | Some time ->
    input_line (Unix.open_process_in (sprintf "date -r %d +%%Y-%%m" time))

let t = Github.(Monad.(run (
  embed (get_auth_token_from_jar "test")
  >>= fun auth ->
  let token = Token.of_auth auth in
  let contributor_stats = Stats.contributors ~token ~user ~repo () in
  Stream.to_list contributor_stats
  >|= List.rev
  >>= function
  | [] ->
    eprintf "No contributors found OR data not yet computed and cached.";
    return ()
  | stats ->
    printf "login : total commits in %s/%s : last month of contribution\n"
      user repo;
    List.iter (fun c ->
      printf "%s : %d : %s\n"
        c.repo_contributor_stats_author.user_login
        c.repo_contributor_stats_total
        (month_of_time_opt (last_seen c.repo_contributor_stats_weeks))
    ) stats;
    return ()
)))

;;

try Lwt_main.run t
with Github.Message (_, message) ->
  eprintf "GitHub API error: %s\n" (Github.API.string_of_message message);
  exit 1
