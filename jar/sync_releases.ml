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

let ask_github fn = Github.(Monad.run (fn ()))

let sync_releases token src_user src_repo dst_user dst_repo =
  lwt src = ask_github (Github.Release.for_repo ~token ~user:src_user ~repo:src_repo) in
  lwt dst = ask_github (Github.Release.for_repo ~token ~user:dst_user ~repo:dst_repo) in
  lwt src_tags = ask_github (Github.Repo.tags ~token ~user:src_user ~repo:src_repo) in
  let open Github_t in
  Lwt_list.iter_s (fun r ->
      let tag = List.find (fun x -> x.repo_tag_name = r.release_tag_name) src_tags in
      let target = match r.release_target_commitish with None -> "master" | Some t -> t in
      let sha = tag.repo_tag_commit.repo_commit_sha in
      let name = match r.release_name with Some name -> name | None -> "NULL" in
      printf "%s %s %s %b %b\n" 
        r.release_tag_name 
        sha
        name r.release_draft r.release_prerelease;
      let release = { 
        new_release_tag_name=r.release_tag_name;
        new_release_target_commitish=sha;
        new_release_name=r.release_name;
        new_release_body=r.release_body;
        new_release_draft=r.release_draft;
        new_release_prerelease=r.release_prerelease;
      } in
      print_endline (Github_j.string_of_new_release release);
      lwt _r = ask_github (Github.Release.create ~token ~user:dst_user ~repo:dst_repo ~release) in
      return ()
    ) src

let run token src_user src_repo dst_user dst_repo =
  Lwt_main.run (sync_releases token src_user src_repo dst_user dst_repo)

let cmd =
  let cookie = Jar_cli.cookie () in
  let src_user = 
    let doc = "The source user name on GitHub" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC_USER" ~doc)
  in
  let src_repo = 
    let doc = "The source repository on GitHub" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"SRC_REPO" ~doc)
  in
  let dst_user = 
    let doc = "The destination user name on GitHub" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"DST_USER" ~doc)
  in
  let dst_repo = 
    let doc = "The destination repository on GitHub" in
    Arg.(required & pos 3 (some string) None & info [] ~docv:"DST_REPO" ~doc)
  in
  let doc = "synchronize releases between GitHub repositories" in
  let man = [ `S "BUGS"; `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";] in
  Term.((pure run $ cookie $ src_user $ src_repo $ dst_user $ dst_repo)),
  Term.info "git-sync-releases" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
