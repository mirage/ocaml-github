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
let version = "1.0.0"

let run_github fn = Lwt_main.run (Github.Monad.run (fn ()))

let scope =
  let parse s =
    match Github.Scope.scope_of_string s with
    |None -> `Error "unknown scope"
    |Some s -> `Ok s in
  let print f s = Format.pp_print_string f (Github.Scope.string_of_scope s) in
  parse, print

let list_auth user pass =
  let open Github_t in
  let auths = run_github (Github.Token.get_all ~user ~pass) in
  List.iter (fun a ->
    Printf.printf "%40s | %25s | %s\n" a.auth_app.app_name a.auth_token (match a.auth_note with None -> "" |Some b -> b)
  ) auths

let make_auth user pass scopes note note_url client_id client_secret =
  let open Github_t in
  let token = run_github (Github.Token.create ~scopes ~note ?note_url 
   ?client_id ?client_secret ~user ~pass) in
  Printf.printf "Created token: %s\n" (Github.Token.to_string token)

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

let default_cmd = 
  let doc = "manipulate Github authorizations" in 
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info "git-jar" ~version ~doc
       
let cmds = [list_cmd; make_cmd]

let () =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1 | _ -> exit 0
