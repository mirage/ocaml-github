open Lwt
open Printf

let token = Config.access_token

let t =
  let open Github.Issues in
  lwt issues  = repo ~token ~user:"avsm" ~repo:"mirage" () in
  match issues with
  |Github.Response r ->
    List.iter (fun issue ->
      eprintf "issue %d: %s\n%!" issue.number issue.title
    ) r;
    return ()
  |Github.Error e ->
    prerr_endline (Github.error_to_string e);
    return ()

let _ = Lwt_main.run t
