
let token = Config.access_token
let user = "docker"
let repo = "for-mac"
let num = 1131

let t =
  let open Github in
  let open Monad in
  run (
    let issue_events = Issue.events ~token ~user ~repo ~num () in
    Stream.to_list issue_events >>= fun _ ->
    return ()
  )

;;
Lwt_main.run t
