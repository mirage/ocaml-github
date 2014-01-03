open Lwt
open Printf

let token = Config.access_token

let print_releases m =
  List.iter (fun m ->
    let open Github_t in
    eprintf "release %d: %s (%s)\n%!" m.release_id m.release_name m.release_created_at
  ) m;
  eprintf "--\n%!"
 
let t =
  let opro_releases = Github.Release.for_repo ~user:"ocaml" ~repo:"opam" in
  Github.(Monad.run (opro_releases ())) >|= print_releases >>
  Github.(Monad.run (Release.for_repo ~user:"mirage" ~repo:"ocaml-cohttp" ())) >|= print_releases >>
  return ()

let _ = Lwt_main.run t
