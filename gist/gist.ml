(*
 * Copyright (c) 2014 Andy Ray <andy.ray@ujamjar.com>
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

(* Utility for working with gist files *)

open Cmdliner
open Printf
open Lwt

open Github_t
module G = Github
module Gist = Github.Gist
module M = Github.Monad

let gist_version = "0.1.0"

let very_pretty_json s = Json.to_string (Yojson.Safe.from_string s :> Yojson.t)
let quite_pretty_json s = Yojson.Safe.pretty_to_string (Yojson.Safe.from_string s)
let pretty_json pretty = if pretty then very_pretty_json else quite_pretty_json

exception Auth_token_not_found of string
exception Gist_file_not_found of string

(************************************************************************)
(* Authorization *)

(* for now, we look it up in the cookie jar.
   We could query github for it instead. *)
let get_auth_token_from_jar auth_id =
  Github_cookie_jar.init () >>= fun jar ->
  Github_cookie_jar.(get jar ~name:auth_id) >>= function
  | Some x -> return x
  | None -> Lwt.fail (Auth_token_not_found "given id not in cookie jar")

(* TODO factor out 2FA code *)
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

(* find a personal access token with either the given name,
 * or the first one to include Gist scope *)
let get_personal_access_token_from_github user pass token_name =
  Passwd.get_if_unset ~prompt:"Enter Github password: " pass >>= fun pass ->
  M.run (complete_2fa (G.Token.get_all ~user ~pass)) >>= fun tokens ->
  try
    match token_name with
    | "" -> (* find token with gist scope *)
      return (List.find (fun a -> List.mem `Gist a.auth_scopes) tokens)
    | _ -> (* find given token *)
      return (List.find (fun a -> a.auth_app.app_name = token_name) tokens)
  with _ ->
    fail (Auth_token_not_found "couldn't find a matching token")

let get_auth auth_id user pass token_name =
  match auth_id, user with
  | "", "" -> Lwt.fail (Auth_token_not_found "must specify username or jar token id")
  | _, "" -> get_auth_token_from_jar auth_id
  | "", _ -> get_personal_access_token_from_github user pass token_name
  | _ -> Lwt.fail (Auth_token_not_found "must specify either username or jar token id")

let login auth_id user pass token_name json pretty =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    if json then
      Lwt_io. printf "%s\n" (pretty_json pretty (Github_j.string_of_auth code))
    else
      return_unit
  )

(************************************************************************)
(* List gists *)

let describe_gist g =
  printf "%20s" g.gist_id;
  (match g.gist_description with
  | Some(d) when d <> "" -> printf " '%s'" d
  | _ -> ());
  printf "\n"

let list_your_gists auth_id user pass token_name json pretty =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    let token = G.Token.of_auth code in
    M.run (G.Stream.to_list (Gist.all ~token ())) >>= fun gists ->
    if json then Lwt_io.printf "%s" (pretty_json pretty (Github_j.string_of_gists gists))
    else return (List.iter describe_gist gists)
  )

let list_user_gists auth_id user pass token_name json pretty username =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    let token = G.Token.of_auth code in
    M.run (G.Stream.to_list (Gist.for_user ~token ~user:username ()))
    >>= fun gists ->
    if json then Lwt_io.printf "%s" (pretty_json pretty (Github_j.string_of_gists gists))
    else return (List.iter describe_gist gists)
  )

(************************************************************************)
(* Post gists *)
let post_gist auth_id user pass token_name _json _pretty new_gist_public new_gist_description files =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    let token = G.Token.of_auth code in
    (* get file contents *)
    let contents fname =
      Lwt_io.(with_file ~mode:input fname read) >>= fun new_gist_content ->
      Lwt.return (fname, {Github_t.new_gist_content})
    in
    Lwt_list.map_s contents files >>= fun new_gist_files ->
    let gist = {
      Github_t.new_gist_files;
      new_gist_description;
      new_gist_public;
    } in
    M.(run (Gist.create ~token ~gist () >|= G.Response.value)) >>= fun gist ->
    return (describe_gist gist)
  )

(************************************************************************)
(* gists file info *)

let string_of_public = function true -> "public" | false -> "private"
let string_of_size x =
  let rnd x y = (x + y - 1) / y in
  if x < 1024 then sprintf "%i B" x
  else if x < (1024*1024) then sprintf "%i KiB" (rnd x 1024)
  else if x < (1024*1024*1024) then sprintf "%i MiB" (rnd x (1024*1024))
  else "> GiB!"
let string_of_bool ?(t="true") ?(f="false") =
  function true -> t
         | false-> f
let string_of_bool_opt ?(t="true") ?(f="false") =
  function Some true -> t
         | Some false | None -> f
let string_of_opt =
  function Some x -> x
         | None -> ""

let comma_sep x =
  List.fold_left
    (fun a x ->
      match a,x with
      | "","" -> ""
      | "",_ -> x
      | _,"" -> a
      | _ -> a ^ "," ^ x)
    "" (List.filter ((<>) "") x)

let print_gist_file_info name file =
  let flags = comma_sep [
    string_of_bool_opt ~t:"truncated" ~f:"" file.gist_file_truncated;
    file.gist_file_ty;
    string_of_opt file.gist_file_language;
  ] in
  Lwt_io.printf "%-40s %-8s %s\n" name (string_of_size file.gist_file_size) flags

let gist_info auth_id user pass token_name json pretty gist_id =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    let token = G.Token.of_auth code in
    M.(run (Gist.get ~token ~id:gist_id () >|= G.Response.value))
    >>= fun gist ->
    if json then
      Lwt_io. printf "%s\n" (pretty_json pretty (Github_j.string_of_gist gist))
    else
      Lwt_list.iter_s
        (fun (name,file) -> print_gist_file_info name file)
        gist.gist_files
  )

let gist_file_info auth_id user pass token_name json pretty gist_id file =
  Lwt_main.run (
    get_auth auth_id user pass token_name >>= fun code ->
    let token = G.Token.of_auth code in
    M.(run (Gist.get ~token ~id:gist_id () >|= G.Response.value))
    >>= fun gist ->
    (try Lwt.return (List.assoc file gist.gist_files)
     with _ -> Lwt.fail (Gist_file_not_found file)) >>=
    fun file_data ->
    if json then
      Lwt_io. printf "%s\n" (pretty_json pretty (Github_j.string_of_gist_file file_data))
    else begin
      print_gist_file_info file file_data >>= fun () ->
      Lwt_io.printf "url: %s\n" file_data.gist_file_raw_url
    end
  )

(************************************************************************)
(* gists files *)

let gist_get _auth_id _user _pass _token_name _json _pretty _gist_id _file_or_dir =
  ()

(************************************************************************)
(* *)

(************************************************************************)
(* user interface *)

let user = Arg.(value & opt string "" & info ["u";"username"] ~docv:"USERNAME"
                  ~doc:"Authentication username.")

let pass = Arg.(value & opt (some string) None & info ["p";"password"]
                  ~docv:"PASSWORD" ~doc:"Authentication password.")

let auth_id = Arg.(value & opt string "" & info ["a";"auth-id"] ~docv:"AUTH-ID"
                  ~doc:"GitHub cookie jar token name.")

let token_name = Arg.(value & opt string "" & info ["t";"token-name"] ~docv:"TOKEN-NAME"
                  ~doc:"Personal authentication token name.")

let json = Arg.(value & flag & info ["json"] ~docv:"JSON"
                 ~doc:"Show JSON responses.")

let pretty = Arg.(value & flag & info ["pretty"] ~docv:"PRETTY"
                 ~doc:"Pretty print JSON responses.")

let user_pos = Arg.(required & pos 0 (some string) None & info [] ~docv:"USERNAME"
                  ~doc:"Github username.")

let gist_id_pos = Arg.(required & pos 0 (some string) None & info [] ~docv:"GIST-ID"
                  ~doc:"GIST id.")

let file_pos = Arg.(required & pos 1 (some string) None & info [] ~docv:"FILENAME"
                  ~doc:"File name.")

let list_your_gists =
  Term.(pure list_your_gists $
    auth_id $ user $ pass $ token_name $ json $ pretty
  ),
  Term.info "list" ~doc:"list your GISTs"

let list_user_gists =
  Term.(pure list_user_gists $
    auth_id $ user $ pass $ token_name $ json $ pretty $ user_pos
  ),
  Term.info "list-user" ~doc:"list users GISTs"

let post_new_gist =
  let public = Arg.(value & flag & info ["public"] ~docv:"PUBLIC GIST"
    ~doc:"Create a public gist (default is secret)") in
  let descr = Arg.(required & opt (some string) None & info ["d";"descr"] ~docv:"DESCRIPTION"
    ~doc:"Description of the Gist") in
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILES") in
  Term.(pure post_gist $
    auth_id $ user $ pass $ token_name $ json $ pretty $ public $ descr $ files),
  Term.info "create" ~doc:"create new gist"

let login =
  Term.(pure login $
    auth_id $ user $ pass $ token_name $ json $ pretty
  ),
  Term.info "login" ~doc:"show login token"

let gist_info =
  Term.(pure gist_info $
    auth_id $ user $ pass $ token_name $ json $ pretty $ gist_id_pos
  ),
  Term.info "info" ~doc:"display info about a given gist"

let gist_file_info =
  Term.(pure gist_file_info $
    auth_id $ user $ pass $ token_name $ json $ pretty $ gist_id_pos $ file_pos
  ),
  Term.info "file-info" ~doc:"display info about a file within gist"

let default_cmd =
  let doc = "manipulate Github GIST files from the command line" in
  Term.(ret (pure (`Help (`Pager, None)))),
  let man = [
    `S "DESCRIPTION";
    `P "Read, write and otherwise manipulate Github GIST files from the command line.  Github authentication is handled with tokens created with the $(b,git-jar) command line tool.";
    `S "AUTHORIZATION OPTIONS";
    `P "An authorization token is required to access the Github API.  You can generate and store \
        a token using the $(b,git-jar) tool and retrieve it with $(b,--auth-id).  Alternatively you can supply a username and password to retrieve a token from Github (specified with $(b,--token-id) or otherwise found automatically).";
    `P "$(b,--username) specify Github username.";
    `P "$(b,--password) optionally specifies the Github password on the command-line. If it isn't present, then the password will be obtained interactively.";
    `P "$(b,--token-id) specify the Github token id.";
    `P "$(b,--auth-id) specify the $(b,git-jar) cookie id.";
    `S "COMMON OPTIONS";
    `P "$(b,--help) will show more help for each of the sub-commands above.";
    `P "$(b,--json) Show JSON responses.";
    `P "$(b,--pretty) pretty print JSON responses.";
    `S "BUGS";
     `P "Email bug reports to <mirageos-devel@lists.xenproject.org>, or report them online at <http://github.com/mirage/ocaml-github/issues>." ] in
  Term.info "git-gist" ~version:gist_version ~doc ~man

let cmds = [list_your_gists; list_user_gists; login; gist_info; gist_file_info; post_new_gist]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1 | _ -> exit 0
