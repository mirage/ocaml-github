open Lwt
open Printf

let token = Config.access_token

let print_statuses sl =
  List.iter (fun s ->
    let open Github_t in
    eprintf "status %d: %s %s %s %s %s\n%!"
      s.status_id
      (Github_j.string_of_status_state s.status_state)
      (match s.status_target_url with None -> "\"\"" | Some x -> x)
      (match s.status_description with None -> "\"\"" | Some x -> x)
      s.status_created_at
      s.status_url
  ) sl
 
let t =
  let user = "dsheets" in
  let repo = "opam-repository" in
  let sha_a = "85e86c0260dd230b6bd37c17056f8282011baf51" in
  let sha_b = "4e89aa7f781c6f094d17079ecb6ca875327eddb8" in
  Github.(Monad.(run (
    let status = Github_t.({
      new_status_state=`Error;
      new_status_target_url=Some ("http://example.com/commit/#"^sha_a);
      new_status_description=Some "error error on the wall";
    }) in
    Repo.create_status ~token ~user ~repo ~sha:sha_a ~status ()
    >>= fun status ->
    Repo.statuses ~token ~user ~repo ~sha:sha_a ()
    >>= fun statuses ->
    print_statuses statuses;
    let status = Github_t.({
      new_status_state=`Pending;
      new_status_target_url=Some ("http://example.com/commit/#"^sha_a);
      new_status_description=Some "append be pend see pend depend";
    }) in
    Repo.create_status ~token ~user ~repo ~sha:sha_a ~status ()
    >>= fun status ->
    Repo.statuses ~token ~user ~repo ~sha:sha_a ()
    >>= fun statuses ->
    print_statuses statuses;
    return ()
  )))

let _ = Lwt_main.run t
