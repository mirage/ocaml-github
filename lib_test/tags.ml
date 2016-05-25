open Lwt
open Printf
open Github_t

let get_auth_token_from_jar auth_id =
  Github_cookie_jar.init ()
  >>= fun jar ->
  Github_cookie_jar.get jar auth_id
  >>= function
  | Some auth -> return auth
  | None -> Lwt.fail (Failure ("id '"^auth_id^"' not in cookie jar"))

let get_tags_and_times ~user ~repo =
  let stream = Github.Repo.get_tags_and_times ~user ~repo () in
  Github.Stream.iter (fun (k,v) ->
    eprintf "%s %s\n" k v;
    Github.Monad.return ()
  ) stream

;;
Lwt_main.run Github.Monad.(run (
  embed (get_auth_token_from_jar "test")
  >>= fun auth ->
  Github.(API.set_token (Token.of_auth auth))
  >>= fun () ->
  get_tags_and_times ~user:"dsheets" ~repo:"axtls"
  >>= fun () ->
  get_tags_and_times ~user:"ocaml" ~repo:"opam"
  >>= fun () ->
  get_tags_and_times ~user:"mirage" ~repo:"ocaml-cstruct"
))
