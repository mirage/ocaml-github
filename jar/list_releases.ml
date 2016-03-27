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
  let name = match r.release_name with Some name -> name | None -> "NULL" in
  printf "### %s-%s: %s\n\n" repo r.release_tag_name name;
  printf "Released on %4d-%02d-%02d as [%s](%s). See <https://github.com/%s/%s> for full history.\n\n"
    (tm.Unix.tm_year+1900) (tm.Unix.tm_mon+1) tm.Unix.tm_mday r.release_tag_name r.release_html_url user repo;
  match r.release_body with
   | None -> printf "NULL\n\n"; return_unit
   | Some "" -> return_unit
   | Some body ->
      printf "%s\n\n" body;
      return ()

let releases_to_json rs =
  print_endline (
    Github_j.string_of_release_repos (
      List.map (fun (release_repo_user,release_repo_repo,release_repo_release) ->
        { Github_j.release_repo_user;release_repo_repo;release_repo_release }
      ) rs)
  )

let list_releases token repos json =
  let repos = List.map (fun r ->
    match Stringext.split ~max:2 ~on:'/' r with
    | [user;repo] -> (user,repo)
    | _ -> eprintf "Repositories must be in username/repo format (e.g. mirage/ocaml-cohttp\n"; exit 1
  ) repos in
  (* Get the releases per repo *)
  begin
    Lwt_list.fold_left_s (fun a (user,repo) ->
      Github.(Monad.(run (
        let releases = Release.for_repo ~token ~user ~repo () in
        Stream.to_list releases
      ))) >>= fun r ->
      return ((List.map (fun r -> (user,repo,r)) r) @ a)) [] repos
  end >>= fun releases ->
  (* Sort them by tag creation date *)
  let rtime (_,_,r) = fst (parse_iso8601_from_github r.Github_t.release_created_at) in
  let releases = List.sort (fun b a -> compare (rtime a) (rtime b)) releases in
  match json with
  | false -> Lwt_list.iter_s release_to_markdown releases
  | true -> releases_to_json releases; return_unit

let cmd =
  let cookie = Jar_cli.cookie () in
  let repos = Jar_cli.repos ~doc_append:" to scan for changelogs" () in
  let doc = "list releases on GitHub repositories" in
  let man = [ `S "BUGS"; `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";] in
  let json =
    let doc = "Output in JSON format." in
    Arg.(value & flag & info ["json"] ~doc) in
  Term.((pure (fun t r j -> Lwt_main.run (list_releases t r j )) $ cookie $ repos $ json)),
  Term.info "git-list-releases" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
