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

open Cmdliner

module T = Github_t

let help_sections = [
  `S "BUGS";
  `P "Email bug reports to <mirageos-devel@lists.xenproject.org>.";
]

let print_repository ({
  T.repository_full_name;
  repository_description;
  repository_stargazers_count;
  repository_language;
  repository_html_url;
  _
}) =
  let language = match repository_language with
    | None -> ""
    | Some lang -> " ("^lang^")"
  in
  Lwt_io.printf "%s%s [%d stars]\n<%s>\n%s\n\n" repository_full_name language
    repository_stargazers_count
    repository_html_url
    (match repository_description with None -> "" | Some d -> d)

let search token ?language ?sort keywords =
  let basic_qs = [`In [`Name; `Description; `Readme]] in
  let qualifiers = match language with
    | None -> basic_qs
    | Some lang -> (`Language lang)::basic_qs
  in
  Github.(Monad.(run (
    let results = Github.Search.repos ~token ?sort ~qualifiers ~keywords () in
    Stream.next results (* TODO: option for count? *)
    >>= function
    | Some ({ T.repository_search_items;
              repository_search_total_count; _ }, _) ->
      embed (Lwt_io.printf "%d results returned of %d total\n\n"
               (List.length repository_search_items)
               repository_search_total_count)
      >>= fun () ->
      embed (Lwt_list.iter_s print_repository repository_search_items)
    | None ->
      embed (Lwt_io.printf "No more results.\n\n")
  )))

let repo_cmd =
  let cookie = Jar_cli.cookie () in

  let doc = "sort by stars, forks, updated, or magic (default)" in
  let docv = "SORTBY" in
  let sort = Arg.(value & opt (enum [
    "stars",Some `Stars;
    "forks",Some `Forks;
    "updated",Some `Updated;
    "magic",None;
  ]) None & info ["sort"] ~docv ~doc) in

  let doc = "language filter" in
  let docv = "LANGUAGE" in
  let language = Arg.(
    value & opt (some string) None & info ["language"] ~docv ~doc
  ) in

  let doc = "keywords" in
  let docv = "KEYWORDS" in
  let keywords = Arg.(value & pos_all string [] & info [] ~docv ~doc) in

  let doc = "search GitHub repositories" in
  let man = help_sections in
  let term = Term.((const (fun t language keywords sort ->
    Lwt_main.run (search t ?language ?sort keywords)
  ) $ cookie $ language $ keywords $ sort)) in
  let info = Cmd.info "repo" ~version:Jar_version.t ~doc ~man in
  Cmd.v info term

let group =
  let doc = "search GitHub" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, git search) searches GitHub for repositories, code, \
         issues, or users.");
  ] @ help_sections in
  let no_cmd_err = `Error (true, "No search object type given.") in
  let default = Term.(ret (const no_cmd_err)) in
  let info = Cmd.info "git-search" ~doc ~man in
  Cmd.group ~default info [repo_cmd]

let () = exit @@ Cmd.eval group
