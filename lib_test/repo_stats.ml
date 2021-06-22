open Lwt
open Printf

let token = Config.access_token
let user = "mirage"
let repo = "ocaml-github"

let t =
  let open Github in

  let pp_sep fmt () = Format.fprintf fmt "; " in

  let print_yearly_stats stats =
    Format.printf "Yearly commit activity.@.";

    List.iter (fun x ->
        Format.printf "{ days: [ %a ]"
          (Format.pp_print_list ~pp_sep Format.pp_print_int) x.Github_t.commit_activity_days;
        Format.printf ", total: %i, week: %i }@." x.Github_t.commit_activity_total x.Github_t.commit_activity_week) stats in

  let print_weekly_activity stats =
    let week x = Format.printf " [ %a ]@." (Format.pp_print_list ~pp_sep Format.pp_print_int) x in

    Format.printf "Testing weekly commit activity.@.";
    List.iter (fun x -> week x) stats in

  let print_weekly_count weekly_count =
    Format.printf "Testing weekly commit count.@.";
    Format.printf " all: [ %a ]@."
      (Format.pp_print_list ~pp_sep Format.pp_print_int)
      weekly_count.Github_t.participation_all;
    Format.printf " owner: [ %a ]@."
      (Format.pp_print_list ~pp_sep Format.pp_print_int)
      weekly_count.Github_t.participation_owner in

  let print_hourly_stats stats =
    let week x = Format.printf " [ %a ]@." (Format.pp_print_list ~pp_sep Format.pp_print_int) x in

    Format.printf "Testing hourly commit activity.@.";
    List.iter (fun x -> week x) stats in

  Monad.(run (
             let frequency = Stats.yearly_commit_activity ~token ~user ~repo () in
             Stream.to_list frequency
  )) >>= function
  | [] -> printf "No yearly stats found OR data not yet computed and cached."; return ()
  | yearly_stats -> print_yearly_stats yearly_stats;

  Monad.(run (
    let frequency = Stats.weekly_commit_activity ~token ~user ~repo () in
    Stream.to_list frequency)) >>= function
  | [] -> printf "No contributors found OR data not yet computed and cached."; return ()
  | stats -> print_weekly_activity stats;

  Monad.(run (Stats.weekly_commit_count ~token ~user ~repo () >|= Response.value)) >>= fun weekly_count ->
    print_weekly_count weekly_count;

  Monad.(run (
    let frequency = Stats.hourly_commit_count ~token ~user ~repo () in
    Stream.to_list frequency )) >>= function
    | [] -> printf "No punch cards found OR data not yet computed and cached."; return ()
    | stats -> print_hourly_stats stats;

  return ()
;;

try Lwt_main.run t
with Github.Message (_, message) ->
  eprintf "GitHub API error: %s\n" (Github.API.string_of_message message);
  exit 1
