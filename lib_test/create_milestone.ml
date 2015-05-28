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
  let milestone = {
    Github_t.new_milestone_title="ocaml-github regression milestone";
    new_milestone_state=`Open;
    new_milestone_description=Some "new milestone description";
    new_milestone_due_on=None;
  } in
    
  lwt milestone = 
    Github.(Monad.(run (
    Milestone.create ~token ~user:"avsm" ~repo:"ocaml-github" ~milestone ()
    >|= Response.value
  ))) in

  eprintf "created milestone number %d\n%!" (milestone.Github_t.milestone_number);
  lwt () = Lwt_unix.sleep 5.0 in
  let num = milestone.Github_t.milestone_number in
  eprintf "about to update milestone\n";
  let milestone = {
    Github_t.update_milestone_title=Some "ocaml-github updated title";
    update_milestone_state=None;
    update_milestone_description=Some "about to delete this";
    update_milestone_due_on=None;
  } in
  
  lwt _milestone =
    Github.(Monad.(run (
    Milestone.update ~token ~user:"avsm" ~repo:"ocaml-github" ~milestone ~num ()
    >|= Response.value
  ))) in
  eprintf "updated, sleeping\n";
  lwt () = Lwt_unix.sleep 5.0 in
  lwt () = 
    Github.(Monad.(run (
    Milestone.delete ~token ~user:"avsm" ~repo:"ocaml-github" ~num ()
    >|= Response.value
  ))) in
  return ()

let _ = Lwt_main.run t
