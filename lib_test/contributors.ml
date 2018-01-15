open Printf
open Github_t

let user = "ocaml"
let repo = "opam-repository"

let get_auth_token_from_jar auth_id = Lwt.(
  Github_cookie_jar.init () >>= fun jar ->
  Github_cookie_jar.(get ~name:auth_id jar) >>= function
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

let space_after s = String.init (20 - String.length s) (fun _ -> ' ')

let t = Github.(Monad.(run (
  embed (get_auth_token_from_jar "test")
  >>= fun auth ->
  let token = Token.of_auth auth in
  let contributors = Repo.contributors ~token ~user ~repo () in
  Stream.to_list contributors
  >>= fun contributors ->
  let table = Hashtbl.create 256 in
  List.iter (fun c ->
    Hashtbl.replace table c.contributor_login c.contributor_contributions
  ) contributors;
  let contributor_stats = Stats.contributors ~token ~user ~repo () in
  Stream.to_list contributor_stats
  >|= List.rev
  >>= function
  | [] ->
    eprintf "No contributors found OR data not yet computed and cached.";
    return ()
  | stats ->
    printf "login%s:\ttotal commits in %s/%s\t:\tlast month of contribution\n"
      (space_after "login") user repo;
    List.iter (fun c ->
      match c.repo_contributor_stats_author with
      | Some author ->
        let user = author.user_login in
        let from_table =
          try string_of_int (Hashtbl.find table user)
          with Not_found -> "?"
        in
        let commits =
          sprintf "%d (%s)" c.repo_contributor_stats_total from_table
        in
        printf "%s%s:\t%s%s:\t%s\n"
          user
          (space_after user)
          commits
          (space_after commits)
          (month_of_time_opt (last_seen c.repo_contributor_stats_weeks));
        Hashtbl.remove table user
      | None -> ()
    ) stats;
    let remaining = Hashtbl.fold (fun k v l -> (k, v)::l) table [] in
    let remaining = List.sort (fun (_,x) (_,y) -> compare y x) remaining in
    List.iter (fun (k, v) ->
      let commits = sprintf "! (%d)" v in
      printf "%s%s:\t%s%s:\t?\n"
        k (space_after k)
        commits (space_after commits)
    ) remaining;
    return ()
)))

;;

try Lwt_main.run t
with Github.Message (_, message) ->
  eprintf "GitHub API error: %s\n" (Github.API.string_of_message message);
  exit 1
