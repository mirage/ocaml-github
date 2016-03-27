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

open Lwt
open Printf

let token = Config.access_token

let t =
  let issue = {
    Github_t.new_issue_title="ocaml-github regression test";
    new_issue_body=Some "ocaml-github body";
    new_issue_assignee=Some "avsm";
    new_issue_milestone=None;
    new_issue_labels=[];
  } in

  Github.(Monad.(run (
    Issue.create ~token ~user:"avsm" ~repo:"ocaml-github" ~issue ()
    >|= Response.value
  ))) >>= fun issue ->
  eprintf "created issue number %d\n%!" (issue.Github_t.issue_number);
  return ()

let _ = Lwt_main.run t
