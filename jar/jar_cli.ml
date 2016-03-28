(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
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

open Printf
open Cmdliner

let map f = Term.(app (pure f))

let auth cookie = Lwt.(
  Lwt_main.run (
    Github_cookie_jar.init ()
    >>= fun jar ->
    Github_cookie_jar.get jar cookie
    >|= function
    | None ->
      eprintf "Missing cookie: use git-jar to create cookie `%s`.\n%!" cookie;
      exit 1
    | Some t -> Github.Token.of_string t.Github_t.auth_token
  )
)

let repos ?(doc_append="") () =
  let doc = "Repositories in user/repo format"^doc_append in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"REPOS" ~doc)

let cookie ?(doc_append="") () =
  let doc = "Authentication cookie"^doc_append in
  let env = Arg.env_var "GH_COOKIE" in
  map auth Arg.(value & opt string "infra"
                & info ~env ["c"] ~docv:"COOKIE" ~doc)
