open Lwt
open Printf

let token = Config.access_token
let user = "docker"
let repo = "for-mac"
let num = 1131

let t =
  let open Github in
  let open Monad in
  let open Github_t in
  run (
(*
    Issue.get ~token ~user ~repo ~num () >|= Response.value >>= fun issue ->
    let num = issue.issue_number in
    eprintf "issue %d: %s\n%!" num issue.issue_title;
    let issue_comments = Issue.comments ~token ~user ~repo ~num () in
    Stream.to_list issue_comments
    >>= fun comments ->
    List.iter (fun c ->
      eprintf "  > %Ld: %s\n" c.issue_comment_id c.issue_comment_body
    ) comments;
*)
    let issue_events = Issue.events ~token ~user ~repo ~num () in
    Stream.to_list issue_events >>= fun events ->
    return ()
  )

;;
Lwt_main.run t

