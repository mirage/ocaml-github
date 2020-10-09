open Printf

let token = Config.access_token

let name_of_release = Github_t.(function
  | { release_name=Some name ;_} -> name
  | { release_name=None      ;_} -> "NULL"
)

let latest_release m =
  let open Github_t in
  let name = name_of_release m in
  eprintf "latest release %Ld: %s (%s)\n%!" m.release_id name m.release_created_at;
  eprintf "--\n%!";
  ()

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

let print_release_assets m =
  let open Github_t in
  List.iter (fun x -> eprintf "asset %Ld %s\n%!" x.release_asset_id x.release_asset_url) m;
  eprintf "--\n%!";
  ()

let t = Github.(Monad.(run (
  return (Release.for_repo ~user:"mirage" ~repo:"ocaml-github" ())
  >>= print_releases >>= fun () ->
  (Release.get_latest ~user:"mirage" ~repo:"ocaml-github" () >|= Response.value >>= fun x ->
   latest_release x;
   return x
  )
  >>= fun a ->
  (Release.list_assets ~user:"mirage" ~repo:"ocaml-github" ~id:a.Github_t.release_id ()
   >|= Response.value
   >|= print_release_assets)
  >>= fun _ ->
  return (Release.for_repo ~user:"mirage" ~repo:"mirage" ())
  >>= print_releases
)))

;;

Lwt_main.run t
