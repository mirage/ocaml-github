open Lwt
open Printf
open Github.Monad
open Github_t

let token = Config.access_token
let get_tags_and_times ~user ~repo =
  let stream = Github.Repo.get_tags_and_times ~token ~user ~repo () in
  Github.Stream.iter (fun (k,v) ->
    eprintf "%s %s\n" k v;
    return ()
  ) stream

;;
Lwt_main.run (run (get_tags_and_times ~user:"ocaml" ~repo:"opam"));
Lwt_main.run (run (get_tags_and_times ~user:"mirage" ~repo:"ocaml-cstruct"));
