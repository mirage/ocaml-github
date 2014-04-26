(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Cmdliner
open Printf

let auth =
  Lwt_main.run (
    Github_cookie_jar.init ()
    >>= fun jar ->
    Github_cookie_jar.get jar "infra"
    >|= function
    | None -> eprintf "Use git-jar to create an `infra` cookie first."; exit 1
    | Some t -> t
  )
let token = Github.Token.of_string auth.Github_t.auth_token

let ask_github fn = Github.(Monad.run (fn ()))

let parse_iso8601_from_github t =
  (** This parses just a subset of ISO8601 that GitHub returns: 
        e.g. 2014-02-21T13:39:04Z *)
  Scanf.sscanf t "%4d-%2d-%2dT%2d:%2d:%2dZ" 
    (fun tm_year tm_mon tm_mday tm_hour tm_min tm_sec ->
       (Unix.(mktime {tm_year=tm_year-1900; tm_mon=tm_mon-1; tm_mday; tm_hour; 
                      tm_min; tm_sec; tm_wday=0; tm_yday=0; tm_isdst=false})))

let release_to_markdown (user,repo,r) =
  let open Github_t in
  let (_,tm) = parse_iso8601_from_github r.release_created_at in
  printf "### %s-%s: %s\n\n" repo r.release_tag_name r.release_name;
  printf "Released on %4d-%02d-%02d ([more information](%s)). Changelog:\n\n"
    (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday r.release_html_url;
  print_endline r.release_body;
  print_endline "";
  return ()

let list_releases repos =
  let repos = List.map (fun r ->
      match Stringext.split ~max:2 ~on:'/' r with
      | [user;repo] -> (user,repo)
      | _ -> eprintf "Repositories must be in username/repo format (e.g. mirage/ocaml-cohttp\n"; exit 1
    ) repos in
  (* Get the releases per repo *)
  lwt releases = Lwt_list.fold_left_s (fun a (user,repo) -> 
      lwt r = ask_github (Github.Release.for_repo ~token ~user ~repo) in
      return ((List.map (fun r -> (user,repo,r)) r) @ a)) [] repos in
  (* Sort them by tag creation date *)
  let rtime (_,_,r) = fst (parse_iso8601_from_github r.Github_t.release_created_at) in
  let releases = List.sort (fun b a -> compare (rtime a) (rtime b)) releases in
  Lwt_list.iter_s release_to_markdown releases

let cmd = 
  let repos = 
    let doc = "The repositories (in user/repo format) to scan for changelogs" in
    Arg.(non_empty & pos_all string [] & info [] ~docv:"REPOS" ~doc) in
  let doc = "list releases on GitHub repositories" in
  let man = [ `S "BUGS"; `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";] in
  Term.((pure (fun r -> Lwt_main.run (list_releases r)) $ repos)),
  Term.info "git-list-releases" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
