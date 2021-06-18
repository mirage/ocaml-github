open Printf

let token = Config.access_token

let name_of_release = Github_t.(function
  | { release_name=Some name ;_} -> name
  | { release_name=None      ;_} -> "NULL"
)

let print_releases m = Github.(Monad.(
  Stream.iter (fun m ->
      let open Github_t in
      let name = name_of_release m in
      eprintf "release %Ld: %s (%s)\n%!" m.release_id name m.release_created_at;
      return ()
  ) m
  >>= fun () ->
  eprintf "--\n%!";
  return ()
))

let t = Github.(Monad.(run (
  return (Release.for_repo ~user:"avsm" ~repo:"ocaml-github" ())
  >>= print_releases
  >>= fun () ->
  return (Release.for_repo ~user:"mirage" ~repo:"mirage" ())
  >>= print_releases
)))

;;

Lwt_main.run t
