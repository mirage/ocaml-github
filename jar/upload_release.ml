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

let upload_release token user repo tag content_type filename =
  let open Github_t in
  ask_github (Github.Release.get_by_tag_name ~token ~user ~repo ~tag)
  >>= fun r ->
  let id = r.release_id in
  print_endline (sprintf "uploading to release id %Ld" id);
  begin
    Lwt_io.file_length filename >|= Int64.to_int >>= fun len ->
    let buf = String.create len in
    Lwt_io.with_file ~mode:Lwt_io.input filename
      (fun ic -> Lwt_io.read_into_exactly ic buf 0 len)
    >>= fun () -> return buf
  end >>= fun body ->
  ask_github (Github.Release.upload_asset
                ~token ~user ~repo ~id ~filename ~content_type ~body)
  >>= fun _a ->
  return_unit

let run token user repo tag content_type filename =
  Lwt_main.run (upload_release token user repo tag content_type filename)

let cmd =
  let cookie = Jar_cli.cookie () in
  let user =
    let doc = "The user name on GitHub" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"USER" ~doc)
  in
  let repo =
    let doc = "The repository name on GitHub" in
    Arg.(required & pos 1 (some string) None & info [] ~docv:"REPO" ~doc)
  in
  let tag =
    let doc = "The release tag name on GitHub" in
    Arg.(required & pos 2 (some string) None & info [] ~docv:"TAG" ~doc)
  in
  let filename =
    let doc = "The filename to upload" in
    Arg.(required & pos 3 (some string) None & info [] ~docv:"FILENAME" ~doc)
  in
  let content_type =
    let doc = "The MIME content-type of the file. Defaults to application/octet-stream, but something more specific is recommended." in
    Arg.(value & pos 4 string "application/octet-stream" & info [] ~docv:"CONTENT_TYPE" ~doc)
  in
  let doc = "upload a release asset to a GitHub repository" in
  let man = [ `S "BUGS"; `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";] in
  Term.((pure run $ cookie $ user $ repo $ tag $ content_type $ filename)),
  Term.info "git-upload-release" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
