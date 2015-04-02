(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

module T = Github_t

let ask_github fn = Github.(Monad.run (fn ()))

let string_of_labels labels =
  let names = List.map (fun { T.label_name } -> label_name) labels in
  String.concat ", " names

let print_issue user repo issue =
  let { T.issue_number; issue_title; issue_labels; issue_comments } = issue in
  printf "%s/%s#%d %s\n" user repo issue_number issue_title;
  printf "  Labels: %s\n" (string_of_labels issue_labels);
  printf "  Comments: %d\n" issue_comments;
  return_unit

let list_issues token repos =
  let repos = List.map (fun r ->
    match Stringext.split ~max:2 ~on:'/' r with
    | [user;repo] -> (user,repo)
    | _ -> eprintf "Repositories must be in username/repo format"; exit 1
  ) repos in
  (* Get the issues per repo *)
  Lwt_list.iter_s (fun (user,repo) ->
    ask_github (Github.Issue.for_repo ~token ~state:`Open ~user ~repo)
    >>= fun r ->
    Lwt_list.iter_s (print_issue user repo) r
  ) repos

let cmd =
  let cookie = Jar_cli.cookie () in
  let repos = Jar_cli.repos ~doc_append:" to list open issues" () in
  let doc = "list issues on GitHub repositories" in
  let man = [
    `S "BUGS";
    `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";
  ] in
  Term.((pure (fun t r -> Lwt_main.run (list_issues t r)) $ cookie $ repos)),
  Term.info "git-list-issues" ~version:Jar_version.t ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
