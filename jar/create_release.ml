(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 *                    Markus Mottl <markus.mottl@gmail.com>
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

let create_release ~token
      ~user ~repo ~tag ~release_name ~target_commitish ~body:new_release_body
      ~assets ~content_type ~prerelease ~draft =
  let open Github_t in
  let new_release =
    {
      new_release_tag_name = tag;
      new_release_target_commitish = target_commitish;
      new_release_name = release_name;
      new_release_body;
      new_release_draft = draft;
      new_release_prerelease = prerelease;
    }
  in
  lwt release = Github.(Monad.(run (
    Release.create ~token ~user ~repo ~release:new_release () >|= Response.value
  ))) in
  let id = release.release_id in
  Lwt_list.iter_s (fun filename ->
    lwt len = Lwt_io.file_length filename >|= Int64.to_int in
    let body = Bytes.create len in
    Lwt_io.with_file ~mode:Lwt_io.input filename
      (fun ic -> Lwt_io.read_into_exactly ic body 0 len)
    >>= fun () ->
    lwt _a = Github.(Monad.(run (
      Release.upload_asset
        ~token ~user ~repo ~id ~filename ~content_type ~body ()
      >|= Response.value
    ))) in
    return ()) assets

let run token user repo tag release_name target_commitish body assets
    content_type prerelease draft =
  Lwt_main.run (
    create_release ~token
      ~user ~repo ~tag ~release_name ~target_commitish ~body
      ~assets ~content_type ~prerelease ~draft)

let cmd =
  let cookie = Jar_cli.cookie () in
  let user =
    let doc = "The user name on GitHub." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"USER" ~doc)
  in
  let repo =
    let doc = "The repository on GitHub." in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"REPO" ~doc)
  in
  let tag =
    let doc = "The tag of the release on GitHub." in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"TAG" ~doc)
  in
  let release_name =
    let doc = "The name of the release on GitHub." in
    Arg.(value & pos 3 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let target_commitish =
    let doc =
      "Optional SHA-1 commit hash or branch name associated with a tag."
    in
    let docv = "TARGET_COMMITISH" in
    Arg.(value & opt string "master" & info ["target_commitish"] ~docv ~doc)
  in
  let body =
    let doc = "Optional text describing the contents of the release." in
    Arg.(value & opt (some string) None & info ["body"] ~docv:"BODY" ~doc)
  in
  let assets =
    let doc = "Optional comma-separated list of assets (files) to upload." in
    Arg.(value & opt (list string) [] & info ["assets"] ~docv:"ASSETS" ~doc)
  in
  let content_type =
    let doc = "\
      The MIME content-type of the assets.  Defaults to \
      $(i,application/octet-stream), but something more specific is recommended.  \
      Assets with mixed content types should be uploaded separately using \
      the $(b,git-upload-release) command."
    in
    Arg.(value & opt string "application/octet-stream" & info ["content-type"]
      ~docv:"CONTENT_TYPE" ~doc)
  in
  let prerelease =
    let doc = "Optional prerelease flag (true or false)." in
    Arg.(value & opt bool false & info ["prerelease"] ~docv:"PRERELEASE" ~doc)
  in
  let draft =
    let doc = "Optional draft flag (true or false)." in
    Arg.(value & opt bool false & info ["draft"] ~docv:"DRAFT" ~doc)
  in
  let doc = "create a software release on GitHub" in
  let man =
    [
      `S "BUGS";
      `P "Email bug reports to <mirageos-devel@lists.xenproject.org>."
    ]
  in
  Term.((pure run $ cookie
    $ user $ repo $ tag $ release_name $ target_commitish
    $ body $ assets $ content_type $ prerelease $ draft)),
  Term.info "git-create-release" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
