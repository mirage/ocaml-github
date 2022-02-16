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

let prompt = "Enter Github password: "

(* Cmdliner converter for Github scope lists *)
let scope =
  let parse s =
    match Github.Scope.of_string s with
    |None -> `Error "unknown scope"
    |Some s -> `Ok s in
  let print f s = Format.pp_print_string f (Github.Scope.to_string s) in
  parse, print

let complete_2fa c =
  let rec try_again f = Github.(Monad.(f () >>~ function
  | Result auths -> return auths
  | Two_factor mode ->
    embed (Lwt_io.printf "Enter 2FA code from '%s': " mode)
    >>= fun () ->
    embed (Lwt_io.(read_line stdin))
    >>= fun otp ->
    let otp = Some otp in
    try_again (c ?otp)
  )) in
  let otp = None in
  try_again (c ?otp)

(* Command definitions *)
let list_auth user pass =
  let open Github_t in
  Lwt_main.run (
    Github_cookie_jar.init ()
    >>= fun jar ->
    Passwd.get_if_unset ~prompt pass
    >>= fun (pass : string) ->
    Github.(Monad.(run (complete_2fa (Token.get_all ~user ~pass))))
    >>= fun auths ->
    Github_cookie_jar.get_all jar >>= fun local ->
    printf "%-13s | %-8s | %-40s | %-10s\n" "Cookie Name" "ID" "Application" "Note";
    printf "%s\n" (String.make 80 '-');
    List.iter (fun a ->
      (* Check if this id is local *)
      let id = a.auth_id in
      let localnames = List.fold_left (fun acc (n,a) ->
        if a.auth_id = id then n::acc else acc) [] local in
      let print_line name =
        Printf.printf "%13s | %-8Ld | %-40s | %-10s\n"
          (match name with None -> "<remote>" |Some n -> n)
          a.auth_id a.auth_app.app_name
          (match a.auth_note with None -> "" |Some b -> b)
      in
      match localnames with
      |[] -> print_line None
      |names -> List.iter (fun x -> print_line (Some x)) names
    ) auths;
    return ()
  )

let list_local () =
  Lwt_main.run begin
    Github_cookie_jar.init () >>= fun jar ->
    Github_cookie_jar.get_all jar >|= fun local ->
    List.iter (fun (name, auth) ->
      let open Github_t in
      printf "%-13s | %-8s | %-40s | %-10s\n"
        "Cookie Name" "ID" "Application" "Note";
      printf "%s\n" (String.make 80 '-');
      printf "%13s | %-8Ld | %-40s | %-10s\n"
        name auth.auth_id auth.auth_app.app_name
        (match auth.auth_note with None -> "" | Some s -> s)
    ) local
  end

let make_auth
    user pass name scopes note note_url client_id client_secret fingerprint =
  let open Github_t in
  let note = match note with None -> name | Some note -> note in
  Lwt_main.run (
    Github_cookie_jar.init ()
    >>= fun jar ->
    Passwd.get_if_unset ~prompt pass
    >>= fun pass ->
    Github.Monad.run
      (complete_2fa
         (Github.Token.create
            ~scopes ~note ?note_url ?client_id ?client_secret ?fingerprint
            ~user ~pass
         )
      )
    >>= fun auth ->
    Github_cookie_jar.save jar ~name ~auth
    >>= fun _jar ->
    Printf.printf "Created token %s (%Ld): %s\n"
      name auth.auth_id (Github.Token.(to_string (of_auth auth)));
    return ()
  )

let revoke_auth user pass name_or_id =
  let open Github_t in
  Lwt_main.run (
    Github_cookie_jar.init ()
    >>= fun jar ->
    Github_cookie_jar.get jar ~name:name_or_id
    >>= (function
      | Some auth -> Lwt.return auth.auth_id
      | None ->
        (try Lwt.return (Int64.of_string name_or_id)
         with _ ->
           Printf.eprintf "unknown name or id %s\n" name_or_id;
           exit 1
        ))
    >>= fun id ->
    Passwd.get_if_unset ~prompt pass
    >>= fun pass ->
    Github.Monad.run (complete_2fa (Github.Token.delete ~user ~pass ~id))
    >>= fun () ->
    Github_cookie_jar.get_all jar >>= fun local ->
    Lwt_list.fold_left_s (fun jar (name,a) ->
      if a.auth_id = id then
        Github_cookie_jar.delete jar ~name
      else return jar
    ) jar local >>= fun _ ->
    return_unit
  )

(* Command declarations for Cmdliner *)
let user = Arg.(required & pos 0 (some string) None & info [] ~docv:"USERNAME"
                  ~doc:"GitHub username.")
let pass = Arg.(
  value & opt (some string) None & info ["p";"password"] ~docv:"PASSWORD"
    ~doc:"GitHub password. If not specified, this will be obtained interactively."
)
let name_or_id = Arg.(
  required & pos 1 (some string) None & info [] ~docv:"TOKEN_NAME_OR_ID"
    ~doc:"Cookie name or numeric GitHub token id."
)

let list_local_cmd =
  let term = Term.(const list_local $ const ()) in
  let info = Cmd.info "local" ~doc:"list local active GitHub authorization tokens" in
  Cmd.v info term

let list_cmd =
  let term = Term.(const list_auth $ user $ pass) in
  let info = Cmd.info "show"
               ~doc:"list all active GitHub authorization tokens, including remote ones." in
  Cmd.v info term

let make_cmd =
  let scopes =
    let scopes = Github.Scope.(String.concat ", " (List.map to_string all)) in
    let doc = Printf.sprintf "Comma delimited list of repository scopes. Can be: %s" scopes in
    Arg.(value & opt (list scope) [] & info ["s";"scopes"] ~docv:"SCOPES" ~doc) in
  let note = Arg.(
    value & opt (some string) None & info ["note"] ~docv:"NOTE"
      ~doc:"Informational note to record beside the authorization token"
  ) in
  let note_url = Arg.(
    value & opt (some string) None & info ["url"] ~docv:"URL"
      ~doc:"URL to record beside the authorization token"
  ) in
  let client_id = Arg.(
    value & opt (some string) None & info ["client-id"] ~docv:"CLIENT_ID"
      ~doc:"Optional oAuth client id to register this token with an application."
  ) in
  let client_secret = Arg.(
    value & opt (some string) None & info ["client-secret"] ~docv:"CLIENT_SECRET"
      ~doc:"Optional oAuth client secret to register this token with an application.") in
  let fingerprint = Arg.(
    value & opt (some string) None & info ["fingerprint"]
      ~docv:"FINGERPRINT" ~doc:"Unique token fingerprint"
  ) in
  let tname = Arg.(
    required & pos 1 (some string) None & info [] ~docv:"COOKIE" ~doc:"The local name for the authorization token that applications can look up.") in
  let term = Term.(const make_auth
        $ user $ pass $ tname
        $ scopes $ note $ note_url $ client_id $ client_secret $ fingerprint) in
  let info = Cmd.info "make" ~doc:"create a new GitHub authorization token" in
  Cmd.v info term

let revoke_cmd =
  let term = Term.(const revoke_auth $ user $ pass $ name_or_id) in
  let info = Cmd.info "revoke"
    ~doc:"revoke a remote GitHub authorization token and remove it from the local cookie jar." in
  Cmd.v info term

let cmds =
  let doc = "let local applications use GitHub authorization tokens" in
  let man = [
    `S "DESCRIPTION";
    `P "Applications that want to use GitHub will need to save authorization tokens locally, and $(b,git-jar) provides a CLI interface to manipulate them. GitHub authorization tokens are mapped onto a local $(i,name), and applictions can query that name at runtime to retrieve a token to use in GitHub API calls.";
    `P "All the tokens are stored in $(i,$HOME/.github/jar/<name>), where $(i,<name>) is the local name of the token.";
    `S "COMMON OPTIONS";
    `P "$(b,--password) optionally specifies the GitHub password on the command-line. If it isn't present, then the password will be obtained interactively.";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `S "BUGS";
    `P "Email bug reports to <mirageos-devel@lists.xenproject.org>, or report them online at <http://github.com/mirage/ocaml-github/issues>."] in
  let info = Cmd.info "git-jar" ~version:Jar_version.t ~doc ~man in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group ~default info [list_cmd; list_local_cmd; make_cmd; revoke_cmd]

let () =
  try
    exit @@ Cmd.eval ~catch:false cmds
  with
  | Github.Message (_,m) ->
    eprintf "GitHub API error: %s\n" (Github.API.string_of_message m);
    exit 1
