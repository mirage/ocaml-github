(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
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

let token = Config.access_token
let user = "ocamlot"
let repo = "opam-repository"

let print_hooks label = Github_t.(Github.(Stream.iter (fun hook ->
  eprintf "%s %s hook %Ld created on %s %b detecting %s\n%!"
    label
    (match hook.hook_config with `Web _ -> "web")
    hook.hook_id
    hook.hook_created_at
    hook.hook_active
    (List.fold_left (fun s ev -> s^(Github_j.string_of_event_type ev)^" ")
       "" hook.hook_events);
  Monad.return ()
)))

let make_web_hook_config url secret = Github_t.({
  web_hook_config_url=url;
  web_hook_config_content_type="json";
  web_hook_config_insecure_ssl=false;
  web_hook_config_secret=secret;
})

let make_hook url events = Github_t.({
  new_hook_config=`Web (make_web_hook_config url None);
  new_hook_events=events;
  new_hook_active=true;
})

let get_hooks = Github.Hook.for_repo ~token ~user ~repo ()

let t = Github.(Monad.(run Github_t.(
  API.set_user_agent "create_hook"
  >>= fun () ->
  print_hooks "Present:" get_hooks
  >>= fun () ->
  let hook = make_hook "http://example.com/" [`Push; `PullRequest; `Status] in
  Hook.create ~token ~user ~repo ~hook ()
  >>~ fun hook_a -> print_hooks "Created:" (Stream.of_list [hook_a])
  >>= fun () ->
  let hook = make_hook "http://example.org/"
    [`CommitComment; `IssueComment; `PullRequestReviewComment] in
  Hook.create ~token ~user ~repo ~hook ()
  >>~ fun hook_b -> print_hooks "Created:" (Stream.of_list [hook_b])
  >>= fun () ->
  Hook.get ~token ~user ~repo ~id:hook_b.hook_id ()
  >>~ fun hook -> print_hooks "Just:" (Stream.of_list [hook])
  >>= fun () ->
  Hook.update ~token ~user ~repo ~id:hook.hook_id ~hook:{
    update_hook_config=`Web (make_web_hook_config "http://example.net/" None);
    update_hook_events=Some (`Watch::hook.hook_events);
    update_hook_active=false;
  } ()
  >>~ fun hook -> print_hooks "Updated:" (Stream.of_list [hook])
  >>= fun () ->
  API.set_user_agent "lib_test/create_hook.ml"
  >>= fun () ->
  print_hooks "Retrieved:" get_hooks
  >>= fun () ->
  Hook.delete ~token ~user ~repo ~id:hook.hook_id ()
  >>~ fun () -> print_hooks "Deleted:" (Stream.of_list [hook])
  >>= fun () ->
  print_hooks "Retrieved:" get_hooks
  >>= fun () ->
  Hook.delete ~token ~user ~repo ~id:hook_a.hook_id ()
  >>~ fun () -> print_hooks "Deleted:" (Stream.of_list [hook_a])
  >>= fun () ->
  print_hooks "Present:" get_hooks
)))

;;

Lwt_main.run t
