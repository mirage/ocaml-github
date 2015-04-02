open Lwt
open Printf

let token = Config.access_token

let name_of_release = Github_t.(function
  | { release_name=Some name } -> name
  | { release_name=None      } -> "NULL"
)

let print_releases m =
  List.iter (fun m ->
      let open Github_t in
      let name = name_of_release m in
      eprintf "release %d: %s (%s)\n%!" m.release_id name m.release_created_at
    ) m;
  eprintf "--\n%!"

let ask_github fn = Github.(Monad.run (fn ()))

(** TODO: pull this out into a git-jar release *)
let sync_releases (src_user,src_repo) (dst_user,dst_repo) =
  lwt src = ask_github (Github.Release.for_repo ~token ~user:src_user ~repo:src_repo) in
  lwt dst = ask_github (Github.Release.for_repo ~token ~user:dst_user ~repo:dst_repo) in
  lwt src_tags = ask_github (Github.Repo.tags ~token ~user:src_user ~repo:src_repo) in
  let open Github_t in
  Lwt_list.iter_s (fun r ->
      let tag = List.find (fun x -> x.repo_tag_name = r.release_tag_name) src_tags in
      let _target = match r.release_target_commitish with
        | None -> "master"
        | Some t -> t
      in
      let sha = tag.repo_tag_commit.repo_commit_sha in
      printf "%s %s %s %b %b\n" 
        r.release_tag_name 
        sha
        (name_of_release r) r.release_draft r.release_prerelease;
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

let t =
  let opro_releases = Github.Release.for_repo ~user:"avsm" ~repo:"ocaml-github" in
  ask_github opro_releases >|= print_releases >>
  ask_github (Github.Release.for_repo ~user:"mirage" ~repo:"mirage") >|= print_releases >>
  (* sync_releases ("mirage","ocaml-cohttp") ("avsm","ocaml-cohttp") *)
  return ()

let _ = Lwt_main.run t
