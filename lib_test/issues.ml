open Lwt
open Printf

let token = Config.access_token
let user = "ocaml"
let repo = "opam"

let t =
  let open Github.Monad in
  let open Github_t in
  run (
    Github.Issue.for_repo ~token ~user ~repo ()
    >>= fun issues ->
      let rec iter =
        function
        |[] -> return ()
        |issue::tl ->
          let issue_number = issue.issue_number in
          eprintf "issue %d: %s\n%!" issue_number issue.issue_title;
          Github.Issue.comments ~token ~user ~repo ~issue_number ()
          >>= fun comments ->
            List.iter (fun c -> eprintf "  > %d: %s\n" c.issue_comment_id c.issue_comment_body) comments;
            iter tl
      in iter issues
  )

let _ = Lwt_main.run t
