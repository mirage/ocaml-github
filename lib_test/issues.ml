open Lwt
open Printf

let token = Config.access_token
let user = "ocaml"
let repo = "opam"

let t =
  let open Github in
  let open Monad in
  let open Github_t in
  run (
    let issues = Issue.for_repo ~token ~user ~repo () in
    Stream.iter (fun issue ->
      let num = issue.issue_number in
      eprintf "issue %d: %s\n%!" num issue.issue_title;
      let issue_comments = Issue.comments ~token ~user ~repo ~num () in
      Stream.to_list issue_comments
      >>= fun comments ->
      List.iter (fun c ->
        eprintf "  > %d: %s\n" c.issue_comment_id c.issue_comment_body
      ) comments;
      return ()
    ) issues
  )

;;
Lwt_main.run t
