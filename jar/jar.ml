(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Cmdliner
open Printf
open Lwt

let version = "1.0.0"

let run_github fn = Lwt_main.run (Github.Monad.run (fn ()))

(* Cmdliner converter for Github scope lists *)
let scope =
  let parse s =
    match Github.Scope.scope_of_string s with
    |None -> `Error "unknown scope"
    |Some s -> `Ok s in
  let print f s = Format.pp_print_string f (Github.Scope.string_of_scope s) in
  parse, print

(* Command definitions *)
let list_auth user pass =
  let open Github_t in
  let t = 
    lwt pass = Passwd.get pass in
    lwt auths = Github.Monad.run (Github.Token.get_all ~user ~pass ()) in
    lwt local = Github_cookie_jar.get_all () in
    printf "%-11s | %-8s | %-40s | %-10s\n" "Cookie Name" "ID" "Application" "Note";
    printf "----------------------------------------------------------------------------------\n";
    List.iter (fun a ->
      (* Check if this id is local *)
      let id = a.auth_id in
      let localnames = List.fold_left (fun acc (n,a) ->
        if a.auth_id = id then n::acc else acc) [] local in
      let print_line name =
        Printf.printf "%11s | %-8d | %-40s | %-10s\n"
          (match name with None -> "<remote>" |Some n -> n)
          a.auth_id a.auth_app.app_name
          (match a.auth_note with None -> "" |Some b -> b)
      in
      match localnames with
      |[] -> print_line None
      |names -> List.iter (fun x -> print_line (Some x)) names
    ) auths;
    return ()
  in
  let () = Lwt_main.run t in ()

let make_auth user pass scopes note note_url client_id client_secret =
  let open Github_t in
  let token = run_github (Github.Token.create ~scopes ~note ?note_url 
   ?client_id ?client_secret ~user ~pass) in
  Printf.printf "Created token %d: %s\n" token.auth_id (Github.Token.(to_string (of_auth token)))

let save_auth user pass id name =
  let open Github_t in
  let auth = run_github (Github.Token.get ~user ~pass ~id) in
  let t = Github_cookie_jar.save ~name ~auth in
  Lwt_main.run t

(* Command declarations for Cmdliner *)
let list_cmd =
  let user = Arg.(required & pos 0 (some string) None & info [] ~docv:"USERNAME" ~doc:"Github username") in
  let pass = Arg.(value & opt string "" & info ["p";"password"] ~docv:"PASSWORD" ~doc:"Github password") in
  Term.(pure list_auth $ user $ pass),
  Term.info "show" ~doc:"list active Github authorizations"

let make_cmd =
  let user = Arg.(required & pos 0 (some string) None & info [] ~docv:"USERNAME" ~doc:"Github username") in
  let pass = Arg.(value & opt string "" & info ["p";"password"] ~docv:"PASSWORD" ~doc:"Github password") in
  let scopes =
    let doc = Printf.sprintf "Comma delimited list of repo scopes. Can be: %s" (Github.Scope.(string_of_scopes all)) in
    Arg.(value & opt (list scope) [] & info ["s";"scopes"] ~docv:"SCOPES" ~doc) in
  let note = Arg.(value & opt string "OCaml Github jar" & info ["note"] ~docv:"NOTE" ~doc:"Note to record beside the authorization token") in
  let note_url = Arg.(value & opt (some string) None & info ["url"] ~docv:"URL" ~doc:"URL to record beside the authorization token") in
  let client_id = Arg.(value & opt (some string) None & info ["client-id"] ~docv:"CLIENT_ID" ~doc:"Optional oAuth client id to register this token with an application.") in
  let client_secret = Arg.(value & opt (some string) None & info ["client-secret"] ~docv:"CLIENT_SECRET" ~doc:"Optional oAuth client secret to register this token with an application.") in
  Term.(pure make_auth $ user $ pass $ scopes $ note $ note_url $ client_id $ client_secret),
  Term.info "make" ~doc:"create a new Github authorization"

let save_cmd =
  let user = Arg.(required & pos 0 (some string) None & info [] ~docv:"USERNAME" ~doc:"Github username") in
  let pass = Arg.(value & opt string "" & info ["p";"password"] ~docv:"PASSWORD" ~doc:"Github password") in
  let id = Arg.(required & pos 1 (some int) None & info [] ~docv:"TOKEN_ID" ~doc:"Github token id") in
  let tname = Arg.(required & pos 2 (some string) None & info [] ~docv:"COOKIE" ~doc:"Local cookie name that applications can look up") in
  Term.(pure save_auth $ user $ pass $ id $ tname),
  Term.info "save" ~doc:"save a Github auth to the local cookie jar"

let default_cmd = 
  let doc = "manipulate Github authorizations" in 
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "git-jar" ~version ~doc
       
let cmds = [list_cmd; make_cmd; save_cmd]

let () =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1 | _ -> exit 0
