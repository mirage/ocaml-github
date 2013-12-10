open Lwt
open Printf
open Github.Monad
open Github_t

let token = Config.access_token
let get_tags_and_times ~user ~repo =
  Github.Tag.get_tags_and_times ~token ~user ~repo () >>=
  fun res ->
  List.iter (fun (k,v) -> eprintf "%s %s\n" k v) res;
  return ()
  
let _ = Lwt_main.run (run (get_tags_and_times ~user:"ocaml" ~repo:"opam"))
let _ = Lwt_main.run (run (get_tags_and_times ~user:"mirage" ~repo:"ocaml-cstruct"))
