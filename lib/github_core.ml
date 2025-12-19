(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2016 David Sheets <sheets@alum.mit.edu>
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

let user_agent = "ocaml-github" (* TODO: add version from build system *)

module Make(Env : Github_s.Env)(Time : Github_s.Time)(CL : Cohttp_lwt.S.Client)
= struct

  let string_of_message message =
    message.Github_t.message_message^
    Github_t.(List.fold_left
       (fun s { error_resource; error_field; error_code; error_message; } ->
          let error_field = match error_field with
            | None -> "\"\""
            | Some x -> x
          in
          let error_message = match error_message with
            | None -> "\"\""
            | Some x -> x
          in
          Printf.sprintf
            "%s\n> Resource type: %s\n  Field: %s\n  Code: %s\n  Message: %s"
            s error_resource error_field error_code error_message
       )
       "" message.Github_t.message_errors)

  exception Message of Cohttp.Code.status_code * Github_t.message

  let log_active = ref Env.debug

  let () = Printexc.register_printer (function
    | Message (code, message) ->
      Some (Printf.sprintf "GitHub API error: %s -- %s"
              (Cohttp.Code.string_of_status code) (string_of_message message))
    | _ -> None
  )

  let log fmt =
    Printf.ksprintf (fun s ->
      match !log_active with
      | false -> ()
      | true  -> prerr_endline (">>> GitHub: " ^ s)) fmt

  type rate = Core | Search
  type rates = {
    core   : Github_t.rate option;
    search : Github_t.rate option;
  }

  let empty_rates = { core = None; search = None }

  let rate_table : (string option,rates) Hashtbl.t = Hashtbl.create 4

  module Response = struct
    type redirect =
      | Temporary of Uri.t
      | Permanent of Uri.t
    type 'a t = < value : 'a; redirects : redirect list >

    let value r = r#value

    let redirects r = r#redirects

    let rec final_resource = function
      | [] -> None
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and perm_resource uri = function
      | [] -> Some (Permanent uri)
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and temp_resource uri = function
      | [] -> Some (Temporary uri)
      | (Temporary uri | Permanent uri)::rest -> temp_resource uri rest

    let wrap : ?redirects:redirect list -> 'a -> 'a t =
      fun ?(redirects=[]) v -> object
        method value = v
        method redirects = redirects
      end
  end

  (* Authorization Scopes *)
  module Scope = struct

    let to_string (x : Github_t.scope) = match x with
      | `User -> "user"
      | `User_email -> "user:email"
      | `User_follow -> "user:follow"
      | `Public_repo -> "public_repo"
      | `Repo -> "repo"
      | `Repo_deployment -> "repo_deployment"
      | `Repo_status -> "repo:status"
      | `Delete_repo -> "delete_repo"
      | `Notifications -> "notifications"
      | `Gist -> "gist"
      | `Read_repo_hook -> "read:repo_hook"
      | `Write_repo_hook -> "write:repo_hook"
      | `Admin_repo_hook -> "admin:repo_hook"
      | `Admin_org_hook -> "admin:org_hook"
      | `Read_org -> "read:org"
      | `Write_org -> "write:org"
      | `Admin_org -> "admin:org"
      | `Read_public_key -> "read:public_key"
      | `Write_public_key -> "write:public_key"
      | `Admin_public_key -> "admin:public_key"
      | `Unknown cons -> "unknown:"^cons

    let of_string x : Github_t.scope option =
      match x with
      | "user" -> Some `User
      | "user_email" -> Some `User_email
      | "user_follow" -> Some `User_follow
      | "public_repo" -> Some `Public_repo
      | "repo" -> Some `Repo
      | "repo_deployment" -> Some `Repo_deployment
      | "repo_status" -> Some `Repo_status
      | "delete_repo" -> Some `Delete_repo
      | "notifications" -> Some `Notifications
      | "gist" -> Some `Gist
      | "read:repo_hook" -> Some `Read_repo_hook
      | "write:repo_hook" -> Some `Write_repo_hook
      | "admin:repo_hook" -> Some `Admin_repo_hook
      | "admin:org_hook" -> Some `Admin_org_hook
      | "read:org" -> Some `Read_org
      | "write:org" -> Some `Write_org
      | "admin:org" -> Some `Admin_org
      | "read:public_key" -> Some `Read_public_key
      | "write:public_key" -> Some `Write_public_key
      | "admin:public_key" -> Some `Admin_public_key
      | _ -> None

    let list_to_string scopes =
      String.concat "," (List.map to_string scopes)

    let list_of_string s =
      let scopes = Stringext.split ~on:',' s in
      List.fold_left (fun a b ->
        match a, of_string b with
        | None, _ -> None
        | Some _, None -> None
        | Some a, Some b -> Some (b::a)
      ) (Some []) scopes

    let all = [
      `User; `User_email; `User_follow;
      `Public_repo; `Repo; `Repo_deployment; `Repo_status; `Delete_repo;
      `Notifications; `Gist;
      `Read_repo_hook; `Write_repo_hook; `Admin_repo_hook;
      `Admin_org_hook; `Read_org; `Write_org; `Admin_org;
      `Read_public_key; `Write_public_key; `Admin_public_key;
    ]

    let max = [
      `User; `Public_repo; `Repo; `Delete_repo; `Gist;
      `Admin_repo_hook; `Admin_org; `Admin_org_hook; `Admin_public_key;
    ]
  end

  module URI = struct
    let authorize ?scopes ?redirect_uri ~client_id ~state () =
      let entry_uri = "https://github.com/login/oauth/authorize" in
      let uri = Uri.of_string entry_uri in
      let q = [ "client_id", client_id; "state", state ] in
      let q = match scopes with
      |Some scopes -> ("scope", Scope.list_to_string scopes) :: q
      |None -> q in
      let q = match redirect_uri with
      |Some uri -> ("redirect_uri", Uri.to_string uri) :: q
      |None -> q in
      Uri.with_query' uri q

    let token ~client_id ~client_secret ~code () =
      let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
      let q = [ "client_id", client_id; "code", code; "client_secret", client_secret ] in
      Uri.with_query' uri q

    let api = "https://api.github.com"

    let rate_limit =
      Uri.of_string (Printf.sprintf "%s/rate_limit" api)

    let authorizations =
      Uri.of_string (Printf.sprintf "%s/authorizations" api)

    let authorization ~id =
      Uri.of_string (Printf.sprintf "%s/authorizations/%Ld" api id)

    let check_run_create ~owner ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-runs" api owner repo)

    let check_run ~owner ~repo ~check_run_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-runs/%s" api owner repo check_run_id)

    let check_runs_list_annotation ~owner ~repo ~check_run_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-runs/%s/annotations" api owner repo check_run_id)

    let check_runs_for_ref ~owner ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits/%s/check-runs" api owner repo sha)

    let check_runs_for_id ~owner ~repo ~check_suite_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-suites/%s/check-runs" api owner repo check_suite_id)

    let check_suite_create ~owner ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-suites" api owner repo)

    let check_suite ~owner ~repo ~check_suite_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-suites/%d" api owner repo check_suite_id)

    let check_suite_rerequest ~owner ~repo ~check_suite_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-suites/%d/rerequest" api owner repo check_suite_id)

    let check_suite_preferences ~owner ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/check-suites/preferences" api owner repo)

    let check_suite_list ~owner ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits/%s/check-suites" api owner repo sha)
    
    let user ?user () =
      match user with
      |None -> Uri.of_string (Printf.sprintf "%s/user" api)
      |Some u -> Uri.of_string (Printf.sprintf "%s/users/%s" api u)

    let user_repos ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/repos" api user)

    let repos =
      Uri.of_string (Printf.sprintf "%s/user/repos" api)

    let orgs =
      Uri.of_string (Printf.sprintf "%s/user/orgs" api)

    let repo ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s" api user repo)

    let repo_forks ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/forks" api user repo)

    let repo_issues ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues" api user repo)

    let repo_issue ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d" api user repo num)

    let repo_tags ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/tags" api user repo)

    let repo_tag ~user ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/git/tags/%s" api user repo sha)
    let repo_tag_name ~user ~repo ~tag =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/tags/%s" api user repo tag)
    let repo_branches ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/branches" api user repo)

    let repo_refs ~ty ~user ~repo =
      let suffix =
        match ty with
        |None -> ""
        |Some ty -> "/"^ty
      in
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/git/refs%s" api user repo suffix)

    let repo_commits ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits" api user repo)

    let repo_commit ~user ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits/%s" api user repo sha)

    let repo_commit_status ~user ~repo ~git_ref =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits/%s/status" api user repo git_ref)

    let repo_statuses ~user ~repo ~git_ref =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/statuses/%s" api user repo git_ref)

    let repo_hooks ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks" api user repo)

    let repo_contributors ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/contributors" api user repo)

    let repo_commit_activity ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/stats/commit_activity" api user repo)

    let repo_contributors_stats ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/stats/contributors" api user repo)

    let repo_code_frequency_stats ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/stats/code_frequency" api user repo)
    let repo_participation_stats ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/stats/participation" api user repo)

    let repo_code_hourly_stats ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/stats/punch_card" api user repo)

    let repo_search =
      Uri.of_string (Printf.sprintf "%s/search/repositories" api)

    let repo_search_issues =
      Uri.of_string (Printf.sprintf "%s/search/issues" api)

    let repo_label ~user ~repo ~name =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/labels/%s" api user repo name)

    let repo_labels ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/labels" api user repo)

    let repo_collaborator ~user ~repo ~name =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/collaborators/%s" api user repo name)

    let repo_collaborators ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/collaborators" api user repo)

    let repo_hook ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks/%Ld" api user repo id)

    let repo_hook_ping ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks/%Ld/pings" api user repo id)

    let repo_hook_test ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks/%Ld/tests" api user repo id)

    let repo_pulls ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls" api user repo)

    let pull ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d" api user repo num)

    let _pull_diff_text ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pull/%d.diff" api user repo num)

    let pull_commits ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d/commits" api user repo num)

    let pull_files ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d/files" api user repo num)

    let pull_merge ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d/merge" api user repo num)

    let repo_milestones ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/milestones" api user repo)

    let milestone ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/milestones/%d" api user repo num)

    let milestone_labels ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/milestones/%d/labels" api user repo num)

    let issues_comments ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/comments" api user repo)

    let issue_comments ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" api user repo num)

    let issue_comment ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/comments/%Ld" api user repo id)

    let issue_labels ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/labels" api user repo num)

    let issue_label ~user ~repo ~num ~name =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/labels/%s" api user repo num name)

    let repo_releases ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases" api user repo)

    let repo_release ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/%Ld" api user repo id)

    let repo_release_latest ~user ~repo=
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/latest" api user repo)

    let upload_release_asset ~user ~repo ~id =
      Uri.of_string (
        Printf.sprintf
          "https://uploads.github.com/repos/%s/%s/releases/%Ld/assets"
          user repo id)

    let get_asset ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/assets/%Ld" api user repo id)

    let delete_asset ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/assets/%Ld" api user repo id)

    let get_release_assets ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/%Ld/assets" api user repo id)

    let repo_deploy_keys ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/keys" api user repo)

    let repo_deploy_key ~user ~repo ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/keys/%Ld" api user repo id)

    let repo_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/events" api user repo)

    let repo_issues_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/events"
                       api user repo)

    let repo_issue_events ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/events"
                       api user repo num)

    let issue_timeline ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/timeline"
                       api user repo num)

    let public_events = Uri.of_string (Printf.sprintf "%s/events" api)

    let network_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/networks/%s/%s/events" api user repo)

    let org_hooks ~org =
      Uri.of_string (Printf.sprintf "%s/repos/%s/hooks" api org)

    let org_hook ~org ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/hooks/%Ld" api org id)
 
   let org_hook_ping ~org ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/hooks/%Ld/pings" api org id)

    let org_hook_test ~org ~id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/hooks/%Ld/tests" api org id)

    let org_repos ~org =
      Uri.of_string (Printf.sprintf "%s/orgs/%s/repos" api org)

    let org_events ~org =
      Uri.of_string (Printf.sprintf "%s/orgs/%s/events" api org)

    let org_member_events ~user ~org =
      Uri.of_string (Printf.sprintf "%s/users/%s/events/orgs/%s" api user org)

    let received_events ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/received_events" api user)

    let public_received_events ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/received_events/public"
                       api user)

    let user_events ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/events" api user)

    let public_user_events ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/events/public" api user)

    (* gists (some repetition here we could factor out) *)
    let list_users_gists ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/gists" api user)

    let list_all_public_gists =
      Uri.of_string (Printf.sprintf "%s/gists/public" api)

    let list_starred_gists =
      Uri.of_string (Printf.sprintf "%s/gists/starred" api)

    let gists =
      Uri.of_string (Printf.sprintf "%s/gists" api)

    let gist ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s" api id)

    let gist_commits ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s/commits" api id)

    let gist_star ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s/star" api id)

    let gist_forks ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s/forks" api id)

    let team ~id =
      Uri.of_string (Printf.sprintf "%s/teams/%Ld" api id)

    let org_teams ~org =
      Uri.of_string (Printf.sprintf "%s/orgs/%s/teams" api org)

    let team_repos ~id =
      Uri.of_string (Printf.sprintf "%s/teams/%Ld/repos" api id)

    let user_orgs ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/orgs" api user)

    let emojis =
      Uri.of_string (Printf.sprintf "%s/emojis" api)
  end

  module C = Cohttp
  module CLB = Cohttp_lwt.Body

  module Monad = struct
    open Printf
    open Lwt

    (* Each API call results in either a valid response or
    * an HTTP error. Depending on the error status code, it may
    * be retried within the monad, or a permanent failure returned *)
    type error =
      | Generic of (C.Response.t * string)
      | Semantic of C.Code.status_code * Github_t.message
      | Bad_response of exn * [ `None | `Json of Yojson.Basic.t | `Raw of string ]
    type request = {
      meth: C.Code.meth; uri: Uri.t;
      headers: C.Header.t; body: string;
    }

    type state = {
      user_agent: string option;
      token: string option
    }
    type 'a signal =
      | Request of request * (request -> 'a signal Lwt.t)
      | Response of 'a
      | Err of error
    type 'a t = state -> (state * 'a signal) Lwt.t

    let string_of_message = string_of_message

    let error_to_string = function
      | Generic (res, body) ->
        Lwt.return
          (sprintf "HTTP Error %s\nHeaders:\n%s\nBody:\n%s\n"
             (C.Code.string_of_status (C.Response.status res))
             (String.concat "" (C.Header.to_lines (C.Response.headers res)))
             body)
      | Semantic (_,message) ->
        Lwt.return ("GitHub API error: "^string_of_message message)
      | Bad_response (exn,j) ->
        Lwt.return (sprintf "Bad response: %s\n%s"
          (Printexc.to_string exn)
          (match j with
           |`None -> "<none>"
           |`Raw r -> sprintf "Raw body:\n%s" r
           |`Json j -> sprintf "JSON body:\n%s" (Yojson.Basic.pretty_to_string j)))

    let error err = Err err
    let response r = Response r
    let request ?token:_ ?(params=[]) ({ uri; _ } as req) reqfn =
      let uri = Uri.add_query_params' uri params in
      Request ({req with uri}, reqfn)

    let prepare_headers state headers =
      (* Add User-Agent *)
      let headers =
        C.Header.prepend_user_agent
          headers
          (user_agent^" "^C.Header.user_agent)
      in
      let headers =
        match state.user_agent with
        | None -> headers
        | Some ua -> C.Header.prepend_user_agent headers ua
      in
      (* Add access token *)
      match state.token with
      | None -> headers
      | Some token -> C.Header.add headers "Authorization" ("token " ^ token)

    let prepare_request state req =
      { req with
        headers=prepare_headers state req.headers;
      }

    let rec bind fn x = fun state -> x state >>= function
      | state, Request (req, reqfn) ->
        reqfn (prepare_request state req)
        >>= fun r ->
        bind fn (fun state -> Lwt.return (state, r)) state
      | state, Response r -> fn r state
      | state, ((Err _) as x) -> Lwt.return (state, x)

    let return r = fun state -> Lwt.return (state, Response r)
    let map f m = bind (fun x -> return (f x)) m

    let initial_state = {user_agent=None; token=None}

    let run th = bind return th initial_state >>= function
      | _, Request (_,_) -> Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Err (Semantic (status,msg)) -> Lwt.(fail (Message (status,msg)))
      | _, Err e -> Lwt.(error_to_string e >>= fun err -> fail (Failure err))

    let (>>=) m f = bind f m
    let (>|=) m f = map f m
    let (>>~) m f = m >|= Response.value >>= f

    let embed lw =
      Lwt.(fun state -> lw >>= (fun v -> return (state, Response v)))

    let fail exn _state = Lwt.fail exn

    let catch try_ with_ state =
      Lwt.catch (fun () -> try_ () state) (fun exn -> with_ exn state)
  end

  module Endpoint = struct
    module Version = struct
      type t = Etag of string | Last_modified of string

      let of_headers headers =
        match C.Header.get headers "etag" with
        | Some etag -> Some (Etag etag)
        | None -> match C.Header.get headers "last-modified" with
          | Some last -> Some (Last_modified last)
          | None -> None

      let add_conditional_headers headers = function
        | None -> headers
        | Some (Etag etag) ->
          C.Header.add headers "If-None-Match" etag
        | Some (Last_modified time) ->
          C.Header.add headers "If-Modified-Since" time
    end

    type t = {
      uri     : Uri.t;
      version : Version.t option;
    }

    let empty = { uri = Uri.empty; version = None; }

    let poll_after : (string, float) Hashtbl.t = Hashtbl.create 8

    let update_poll_after uri { C.Response.headers; _ } =
      let now = Time.now () in
      let poll_limit = match C.Header.get headers "x-poll-interval" with
        | Some interval -> now +. (float_of_string interval)
        | None -> now +. 60.
      in
      let uri_s = Uri.to_string uri in
      let t_0 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      if t_0 < poll_limit then Hashtbl.replace poll_after uri_s poll_limit

    let poll_result uri ({ C.Response.headers; _ } as envelope) =
      let version = Version.of_headers headers in
      update_poll_after uri envelope;
      { uri; version; }

    (* TODO: multiple polling threads need to queue *)
    let wait_to_poll uri =
      let now = Time.now () in
      let uri_s = Uri.to_string uri in
      let t_1 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      Monad.embed begin
        if now < t_1
        then Time.sleep (t_1 -. now)
        else Lwt.return_unit
      end
  end

  module Stream = struct
    type 'a t = {
      restart  : Endpoint.t -> 'a t option Monad.t;
      buffer   : 'a list;
      refill   : (unit -> 'a t Monad.t) option;
      endpoint : Endpoint.t;
    }
    type 'a parse = string -> 'a list Lwt.t

    let empty = {
      restart = (fun _endpoint -> Monad.return None);
      buffer = []; refill = None;
      endpoint = Endpoint.empty;
    }

    let rec next = Monad.(function
      | { buffer=[]; refill=None; _ } -> return None
      | { buffer=[]; refill=Some refill; _ } -> refill () >>= next
      | { buffer=h::buffer; _ } as s -> return (Some (h, { s with buffer }))
    )

    let map f s =
      let rec refill s () = Monad.(
        next s
        >>= function
        | None -> return empty
        | Some (v,s) ->
          f v
          >>= function
          | [] -> refill s ()
          | buffer ->
            return { s with restart; buffer; refill = Some (refill s) }
      )
      and restart endpoint = Monad.(
        s.restart endpoint
        >>= function
        | Some s -> return (Some {
          s with restart; buffer = []; refill = Some (refill s);
        })
        | None -> return None
      ) in
      {
        s with
        restart;
        buffer = [];
        refill = Some (refill s);
      }

    let rec fold f a s = Monad.(
      next s
      >>= function
      | None -> return a
      | Some (v,s) ->
        f a v
        >>= fun a ->
        fold f a s
    )

    let rec find p s = Monad.(
      next s
      >>= function
      | None -> return None
      | Some (n,s) as c -> if p n then return c else find p s
    )

    let rec iter f s = Monad.(
      next s
      >>= function
      | None -> return ()
      | Some (v,s) -> f v >>= fun () -> iter f s
    )

    let to_list s =
      let rec aux lst s = Monad.(
        next s
        >>= function
        | None -> return (List.rev lst)
        | Some (v,s) -> aux (v::lst) s
      ) in
      aux [] s

    let of_list buffer = { empty with buffer; refill=None; }

    let poll stream = stream.restart stream.endpoint

    let since stream version =
      { stream with endpoint = {
          stream.endpoint with Endpoint.version = Some version;
        };
      }

    let version stream = stream.endpoint.Endpoint.version
  end

  type 'a authorization =
    | Result of 'a
    | Two_factor of string

  type 'a parse = string -> 'a Lwt.t
  type 'a handler = (C.Response.t * string -> bool) * 'a

  module API = struct
    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response redirects (envelope,body as response) = Lwt.(
      function
      | (p, handler)::more ->
        if not (p response) then handle_response redirects response more
        else
          let bad_response exn body = return (Monad.(error (Bad_response (exn,body)))) in
          catch (fun () ->
            handler response
            >>= fun r ->
            return (Monad.response (Response.wrap ~redirects r))
          ) (fun exn ->
            catch (fun () ->
              catch (fun () ->
                let json = Yojson.Basic.from_string body in
                log "response body:\n%s" (Yojson.Basic.pretty_to_string json);
                bad_response exn (`Json json)
              ) (fun _exn -> bad_response exn (`Raw body))
            ) (fun _exn -> bad_response exn `None)
          )
      | [] ->
        let status = C.Response.status envelope in
        match status with
        | `Unprocessable_entity | `Gone | `Unauthorized | `Forbidden ->
          let message = Github_j.message_of_string body in
          return Monad.(error (Semantic (status,message)))
        | _ ->
          return Monad.(error (Generic (envelope, body)))
    )

    let update_rate_table rate ?token response =
      let headers = C.Response.headers response in
      match C.Header.get headers "x-ratelimit-limit",
            C.Header.get headers "x-ratelimit-remaining",
            C.Header.get headers "x-ratelimit-reset"
      with
      | Some limit_s, Some remaining_s, Some reset_s ->
        let v =
          try Hashtbl.find rate_table token with Not_found -> empty_rates
        in
        let rate_limit = int_of_string limit_s in
        let rate_remaining = int_of_string remaining_s in
        let rate_reset = float_of_string reset_s in
        let new_rate =
          Some { Github_t.rate_limit; rate_remaining; rate_reset }
        in
        let new_rates = match rate with
          | Core -> { v with core = new_rate }
          | Search -> { v with search = new_rate }
        in
        Hashtbl.replace rate_table token new_rates
      | _ -> ()

    (* Force chunked-encoding
     * to be disabled (to satisfy Github, which returns 411 Length Required
     * to a chunked-encoding POST request). *)
    let lwt_req {Monad.uri; meth; headers; body} =
      log "Requesting %s" (Uri.to_string uri);
      let body = CLB.of_string body in
      CL.call ~headers ~body ~chunked:false meth uri

    let max_redirects = 64
    let make_redirect target = function
      | `Moved_permanently -> Response.Permanent target
      | _ -> Response.Temporary target

    let rec request ?(redirects=[]) ~rate ~token resp_handlers req = Lwt.(
      if List.length redirects > max_redirects
      then Lwt.fail (Message (`Too_many_requests, Github_t.{
        message_message = Printf.sprintf
            "ocaml-github exceeded max redirects %d" max_redirects;
        message_errors = [];
      }))
      else
        lwt_req req
        >>= fun (resp, body) ->
        update_rate_table rate ?token resp;
        let response_code = C.Response.status resp in
        log "Response code %s\n%!" (C.Code.string_of_status response_code);
        match response_code with
        | `Found | `Temporary_redirect | `Moved_permanently -> begin
            match C.Header.get (C.Response.headers resp) "location" with
            | None -> Lwt.fail (Message (`Expectation_failed, Github_t.{
              message_message = "ocaml-github got redirect without location";
              message_errors = [];
            }))
            | Some location_s ->
              let location = Uri.of_string location_s in
              let target = Uri.resolve "" req.Monad.uri location in
              let redirect = make_redirect target response_code in
              let redirects = redirect::redirects in
              let req = { req with Monad.uri = target } in
              request ~redirects ~rate ~token resp_handlers req
          end
        | _ ->
          CLB.to_string body >>= fun body ->
          handle_response (List.rev redirects) (resp,body) resp_handlers
    )

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      (fun (res,_) -> C.Response.status res = expected_code), handler

    (* Add the correct mime-type header and the authentication token. *)
    let realize_headers
        ~token
        ?(media_type="application/vnd.github+json")
        headers =
      let headers = C.Header.add_opt headers "accept" media_type in
      let headers = C.Header.add headers "X-GitHub-Api-Version" "2022-11-28" in
      match token with
      | None -> headers
      | Some token -> C.Header.add headers "Authorization" ("Bearer " ^ token)

    let idempotent meth
        ?(rate=Core) ?media_type ?headers ?token ?params ~fail_handlers ~expected_code ~uri
        fn =
      fun state -> Lwt.return
        (state,
         (Monad.(request ?token ?params
                   {meth; uri; headers=realize_headers ~token ?media_type headers; body=""})
            (request ~rate ~token
               ((code_handler ~expected_code fn)::fail_handlers))))

    let just_body (_,(body:string)):string Lwt.t = Lwt.return body

    let effectful meth
        ?(rate=Core) ?headers ?body ?token ?params
        ~fail_handlers ~expected_code ~uri fn =
      let body = match body with None -> ""| Some b -> b in
      let fn x = Lwt.(just_body x >>= fn) in
      let fail_handlers = List.map (fun (p,fn) ->
        p,Lwt.(fun x -> just_body x >>= fn)
      ) fail_handlers in
      fun state -> Lwt.return
        (state,
        (Monad.(request ?token ?params
                  {meth; uri; headers=realize_headers ~token headers; body })
           (request ~rate ~token
              ((code_handler ~expected_code fn)::fail_handlers))))

    let map_fail_handlers f fhs = List.map (fun (p,fn) ->
      p, f fn;
    ) fhs

    let get ?rate
        ?(fail_handlers=[]) ?(expected_code=`OK) ?media_type ?headers
        ?token ?params ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `GET ?rate ~fail_handlers ~expected_code
        ?media_type ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let rec next_link base = Cohttp.Link.(function
    | { context; arc = { Arc.relation; _ }; target }::_
      when Uri.(equal context empty) && List.mem Rel.next relation ->
      Some (Uri.resolve "" base target)
    | _::rest -> next_link base rest
    | [] -> None
    )

    let stream_fail_handlers restart fhs =
      map_fail_handlers Lwt.(fun f (_envelope, body) ->
        f body >>= fun buffer ->
        return {
          Stream.restart; buffer; refill=None; endpoint=Endpoint.empty;
        }
      ) fhs

    let rec stream_next restart request uri fn endpoint (envelope, body) = Lwt.(
      let endpoint = match endpoint.Endpoint.version with
        | None -> Endpoint.poll_result uri envelope
        | Some _ -> endpoint
      in
      let refill = Some (fun () ->
        let links = Cohttp.(Header.get_links envelope.Response.headers) in
        match next_link uri links with
        | None -> Monad.return Stream.empty
        | Some uri -> request ~uri (stream_next restart request uri fn endpoint)
      ) in
      fn body >>= fun buffer ->
      return { Stream.restart; buffer; refill; endpoint }
    )

    let rec restart_stream
        ?rate ~fail_handlers ~expected_code ?media_type ?headers ?token
        ?params fn endpoint =
      let restart = restart_stream
          ?rate ~fail_handlers ~expected_code ?headers ?token ?params fn
      in
      let first_request ~uri f =
        let not_mod_handler =
          code_handler ~expected_code:`Not_modified (fun (envelope,_) ->
            Endpoint.update_poll_after uri envelope;
            Lwt.return_none
          )
        in
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        let fail_handlers = map_fail_handlers Lwt.(fun f response ->
          f response >|= fun stream -> Some stream
        ) fail_handlers in
        let fail_handlers = not_mod_handler::fail_handlers in
        let f ((envelope, _) as response) = Lwt.(
          let endpoint = Endpoint.poll_result uri envelope in
          f response
          >|= fun stream ->
          Some { stream with Stream.endpoint }
        ) in
        let headers = match headers with
          | None -> C.Header.init ()
          | Some h -> h
        in
        let headers =
          Endpoint.(Version.add_conditional_headers headers endpoint.version)
        in
        Monad.(
          Endpoint.wait_to_poll uri
          >>= fun () ->
          idempotent ?rate
            `GET ?media_type ~headers ?token ?params ~fail_handlers
            ~expected_code ~uri f
        )
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate
             `GET ?media_type ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let uri = endpoint.Endpoint.uri in
      Monad.map Response.value
        (first_request ~uri (stream_next restart request uri fn endpoint))

    let get_stream (type a)
        ?rate
        ?(fail_handlers:a Stream.parse handler list=[])
        ?(expected_code:Cohttp.Code.status_code=`OK)
        ?media_type
        ?(headers:Cohttp.Header.t option) ?(token:string option)
        ?(params:(string * string) list option)
        ~(uri:Uri.t) (fn : a Stream.parse) =
      let restart = restart_stream
          ?rate ~fail_handlers ~expected_code ?headers ?token ?params fn
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate
             `GET ?media_type ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let endpoint = Endpoint.({ empty with uri }) in
      let refill = Some (fun () ->
        request ~uri (stream_next restart request uri fn endpoint)
      ) in
      {
        Stream.restart;
        buffer = [];
        refill;
        endpoint;
      }

    let post ?rate ?(fail_handlers=[]) ~expected_code =
      effectful `POST ?rate ~fail_handlers ~expected_code

    let patch ?rate ?(fail_handlers=[]) ~expected_code =
      effectful `PATCH ?rate ~fail_handlers ~expected_code

    let put ?rate ?(fail_handlers=[]) ~expected_code ?headers ?body =
      let headers = match headers, body with
        | None, None -> Some (C.Header.init_with "content-length" "0")
        | Some h, None -> Some (C.Header.add h "content-length" "0")
        | _, Some _ -> headers
      in
      effectful `PUT ?rate ~fail_handlers ~expected_code ?headers ?body

    let delete ?rate
        ?(fail_handlers=[]) ?(expected_code=`No_content) ?headers ?token ?params
        ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `DELETE ?rate
        ~fail_handlers ~expected_code ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let set_user_agent user_agent = fun state ->
      Monad.(Lwt.return ({state with user_agent=Some user_agent}, Response ()))

    let set_token token = fun state ->
      Monad.(Lwt.return ({state with token=Some token}, Response ()))

    let rates_of_resources rate_limit_resources = {
      core = Some rate_limit_resources.Github_t.rate_resources_core;
      search = Some rate_limit_resources.Github_t.rate_resources_search;
    }

    let request_rate_limit ?token () = Monad.(
      let uri = URI.rate_limit in
      get ?token ~uri (fun b -> Lwt.return (Github_j.rate_limit_of_string b))
      >>~ fun { Github_t.rate_limit_resources } ->
      let rates = rates_of_resources rate_limit_resources in
      Hashtbl.replace rate_table token rates;
      return rate_limit_resources
    )

    let cached_rates ?token () =
      try Monad.return (Hashtbl.find rate_table token)
      with Not_found ->
        Monad.map rates_of_resources (request_rate_limit ?token ())

    let get_rate ?(rate=Core) ?token () = Monad.(
      cached_rates ?token ()
      >>= fun rates ->
      let rec get_core_rate = function
        | { core = None; _ } ->
          Monad.map rates_of_resources (request_rate_limit ?token ())
          >>= get_core_rate
        | { core = Some rate; _ } -> return rate
      in
      let rec get_search_rate = function
        | { search = None; _ } ->
          Monad.map rates_of_resources (request_rate_limit ?token ())
          >>= get_search_rate
        | { search = Some rate; _ } -> return rate
      in
      match rate with
      | Core -> get_core_rate rates
      | Search -> get_search_rate rates
    )

    let get_rate_limit ?token () = Monad.(
      get_rate ?token ()
      >>= fun { Github_t.rate_limit; _ } -> return rate_limit
    )

    let get_rate_remaining ?token () = Monad.(
      get_rate ?token ()
      >>= fun { Github_t.rate_remaining; _ } -> return rate_remaining
    )

    let get_rate_reset ?token () = Monad.(
      get_rate ?token ()
      >>= fun { Github_t.rate_reset; _ } -> return rate_reset
    )

    let string_of_message = Monad.string_of_message

  end

  open Github_j

  module Rate_limit = struct
    open Monad

    let all ?token () = API.request_rate_limit ?token ()

    let for_core ?token () =
      all ?token ()
      >>= fun { rate_resources_core; _ } -> return rate_resources_core

    let for_search ?token () =
      all ?token ()
      >>= fun { rate_resources_search; _ } -> return rate_resources_search

  end

  module Token = struct
    open Lwt
    type t = string

    let two_factor_auth_handler () =
      let mode = ref "" in
      (fun (res,_) ->
         C.Response.status res = `Unauthorized
         &&
         match C.Header.get (C.Response.headers res) "x-github-otp" with
         | None -> false
         | Some v ->
           let required = String.sub v 0 10 in
           if required = "required; "
           then (mode := (Stringext.string_after v 10); true)
           else false
      ), (fun _ -> return (Two_factor !mode))

    let add_otp headers = function
        | None -> headers
        | Some code -> C.Header.replace headers "x-github-otp" code

    let create ?(scopes=[`Repo]) ?(note="ocaml-github") ?note_url ?client_id
        ?client_secret ?fingerprint ?otp ~user ~pass () =
      let req = {
        auth_req_scopes=scopes; auth_req_note=note; auth_req_note_url=note_url;
        auth_req_fingerprint=fingerprint;
        auth_req_client_id=client_id; auth_req_client_secret=client_secret;
      } in
      let body = string_of_auth_req req in
      let headers =
        add_otp C.Header.(add_authorization (init ()) (`Basic (user,pass))) otp
      in
      let uri = URI.authorizations in
      let fail_handlers = [two_factor_auth_handler ()] in
      API.post ~headers ~body ~uri ~fail_handlers ~expected_code:`Created
        (fun body -> return (Result (auth_of_string body)))

    let get_all ?otp ~user ~pass () =
      let uri = URI.authorizations in
      let headers =
        add_otp C.Header.(add_authorization (init ()) (`Basic (user,pass))) otp
      in
      let fail_handlers = [two_factor_auth_handler ()] in
      API.get ~headers ~uri ~fail_handlers ~expected_code:`OK (fun body ->
        return (Result (auths_of_string body))
      )

    let get ?otp ~user ~pass ~id () =
      let uri = URI.authorization ~id in
      let headers =
        add_otp C.Header.(add_authorization (init ()) (`Basic (user,pass))) otp
      in
      let fail_handlers = [
        two_factor_auth_handler ();
        API.code_handler ~expected_code:`Not_found
          (fun _ -> return (Result None));
      ] in
      API.get ~headers ~uri ~fail_handlers ~expected_code:`OK (fun body ->
        return (Result (Some (auth_of_string body)))
      )

    let delete ?otp ~user ~pass ~id () =
      let uri = URI.authorization ~id in
      let headers =
        add_otp C.Header.(add_authorization (init ()) (`Basic (user,pass))) otp
      in
      let fail_handlers = [two_factor_auth_handler ()] in
      API.delete ~headers ~uri ~fail_handlers ~expected_code:`No_content
        (fun _body -> return (Result ()))

    (* Convert a code after a user oAuth into an access token that can
       be used in subsequent requests.
       TODO: more informative error case
    *)
    let of_code ~client_id ~client_secret ~code () =
      let uri = URI.token ~client_id ~client_secret ~code () in
      CL.post uri
      >>= fun (_res, body) ->
      CLB.to_string body
      >>= fun body ->
      try
        let form = Uri.query_of_encoded body in
        return (Some (List.(hd (assoc "access_token" form))))
      with _ ->
        return None

    let of_auth x = x.auth_token
    let of_string x = x
    let to_string x = x
  end

  module User = struct
    open Lwt

    let current_info ?token () =
      let uri = URI.user () in
      API.get ?token ~uri (fun body -> return (user_info_of_string body))

    let info ?token ~user () =
      let uri = URI.user ~user () in
      API.get ?token ~uri (fun body -> return (user_info_of_string body))

    let repositories ?token ~user () =
      let uri = URI.user_repos ~user in
      API.get_stream ?token ~uri (fun b ->
        return (repositories_of_string b)
      )

  end

  module Organization = struct
    module Hook = struct
      open Lwt

      let for_org ?token ~org () =
        let uri = URI.org_hooks ~org in
        API.get_stream ?token ~uri (fun b -> return (hooks_of_string b))

      let get ?token ~org ~id () =
        let uri = URI.org_hook ~org ~id in
        API.get ?token ~uri (fun b -> return (hook_of_string b))

      let create ?token ~org ~hook () =
        let uri = URI.org_hooks ~org in
        let body = string_of_new_hook hook in
        API.post ~body ?token ~uri ~expected_code:`Created
          (fun b -> return (hook_of_string b))

      let update ?token ~org ~id ~hook () =
        let uri = URI.org_hook ~org ~id in
        let body = string_of_update_hook hook in
        API.patch ?token ~body ~uri ~expected_code:`OK
          (fun b -> return (hook_of_string b))

      let delete ?token ~org ~id () =
        let uri = URI.org_hook ~org ~id in
        API.delete ?token ~uri (fun _ -> return ())

      let ping ?token ~org ~id () =
        let uri = URI.org_hook_ping ~org ~id in
        API.post ?token ~uri ~expected_code:`Created (fun _ -> return ())

      let test ?token ~org ~id () =
        let uri = URI.org_hook_test ~org ~id in
        API.post ?token ~uri ~expected_code:`No_content (fun _b -> return ())

      let parse_event ~constr ~payload () : Github_t.event_hook_constr =
        let parse_json = function
          | "" -> None
          | s -> Some (Yojson.Safe.from_string s)
        in
        match Github_j.event_type_of_string ("\"" ^ constr ^ "\"") with
        | `CommitComment ->
          `CommitComment (Github_j.commit_comment_event_of_string payload)
        | `CheckRun ->
            `CheckRun (Github_j.check_run_event_of_string payload)
        | `CheckSuite->
            `CheckSuite (Github_j.check_suite_event_of_string payload)
        | `Create ->
          `Create (Github_j.create_event_of_string payload)
        | `Delete ->
          `Delete (Github_j.delete_event_of_string payload)
        | `Deployment ->
          `Unknown ("deployment", parse_json payload)
        | `DeploymentStatus ->
          `Unknown ("deployment_status", parse_json payload)
        | `Download -> `Download
        | `Follow -> `Follow
        | `Fork ->
          `Fork (Github_j.fork_event_of_string payload)
        | `ForkApply -> `ForkApply
        | `Gist -> `Gist
        | `Gollum ->
          `Gollum (Github_j.gollum_event_of_string payload)
        | `IssueComment ->
          `IssueComment (Github_j.issue_comment_event_of_string payload)
        | `Issues ->
          `Issues (Github_j.issues_event_of_string payload)
        | `Member ->
          `Member (Github_j.member_event_of_string payload)
        | `PageBuild ->
          `Unknown ("page_build", parse_json payload)
        | `Public -> `Public
        | `PullRequest ->
          `PullRequest (Github_j.pull_request_event_of_string payload)
        | `PullRequestReviewComment ->
          `PullRequestReviewComment
            (Github_j.pull_request_review_comment_event_of_string payload)
        | `Push ->
          `Push (Github_j.push_event_hook_of_string payload)
        | `Release ->
          `Release (Github_j.release_event_of_string payload)
        | `Repository ->
          `Repository (Github_j.repository_event_of_string payload)
        | `Status ->
          `Status (Github_j.status_event_of_string payload)
        | `TeamAdd ->
          `Unknown ("team_add", parse_json payload)
        | `Watch ->
          `Watch (Github_j.watch_event_of_string payload)
        | `All -> `Unknown ("*", parse_json payload)
        | `Unknown cons -> `Unknown (cons, parse_json payload)
        | `Ping -> `Ping (Github_j.ping_event_of_string payload)

      let parse_event_metadata ~payload () =
        Github_j.event_hook_metadata_of_string payload
    end

    open Lwt

    let teams ?token ~org () =
      let uri = URI.org_teams ~org in
      API.get_stream ?token ~uri (fun b -> return (teams_of_string b))

    let user_orgs ?token ~user () =
      let uri = URI.user_orgs ~user in
      API.get_stream ?token ~uri (fun b -> return (orgs_of_string b))

    let current_user_orgs ?token () =
      let uri = URI.orgs in
      API.get_stream ?token ~uri (fun b -> return (orgs_of_string b))

    let repositories ?token ~org () =
      let uri = URI.org_repos ~org in
      API.get_stream ?token ~uri (fun b ->
          return (repositories_of_string b)
        )
  end

  module Team = struct
    open Lwt

    let info ?token ~id () =
      let uri = URI.team ~id in
      API.get ?token ~uri (fun b -> return (team_info_of_string b))

    let repositories ?token ~id () =
      let uri = URI.team_repos ~id in
      API.get_stream ?token ~uri (fun b -> return (repositories_of_string b))
  end

  module Filter = struct
    type state = [ `All | `Open | `Closed ]
    let string_of_state (s:state) =
      match s with
      |`All -> "all"
      |`Open -> "open"
      |`Closed -> "closed"

    type milestone_sort = [ `Due_date | `Completeness ]
    let string_of_sort (s:milestone_sort) =
      match s with
      |`Due_date -> "due_date"
      |`Completeness -> "completeness"

    type issue_sort = [ `Created | `Updated | `Comments ]
    let string_of_issue_sort (s:issue_sort) =
      match s with
      |`Created -> "created"
      |`Updated -> "updated"
      |`Comments -> "comments"

    type issue_comment_sort = [ `Created | `Updated ]
    let string_of_issue_comment_sort (s:issue_comment_sort) =
      match s with
      |`Created -> "created"
      |`Updated -> "updated"

    type repo_sort = [ `Stars | `Forks | `Updated ]
    let string_of_repo_sort (s:repo_sort) =
      match s with
      |`Stars -> "stars"
      |`Forks -> "forks"
      |`Updated -> "updated"

    type forks_sort = [ `Newest | `Oldest | `Stargazers ]
    let string_of_forks_sort (s:forks_sort) =
      match s with
      |`Newest -> "newest"
      |`Oldest -> "oldest"
      |`Stargazers -> "stargazers"

    type direction = [ `Asc | `Desc ]
    let string_of_direction (d:direction) =
      match d with
      |`Asc -> "asc"
      |`Desc -> "desc"

    type milestone = [ `Any | `None | `Num of int ]
    let string_of_milestone (m:milestone) =
      match m with
      |`Any -> "*"
      |`None -> "none"
      |`Num n -> string_of_int n

    type user = [ `Any | `None | `Login of string ]
    let string_of_user (a:user) =
      match a with
      |`Any -> "*"
      |`None -> "none"
      |`Login u -> u

    type 'a range = [
      | `Range of 'a option * 'a option
      | `Lt of 'a
      | `Lte of 'a
      | `Eq of 'a
      | `Gte of 'a
      | `Gt of 'a
    ]
    let string_of_range str_fn (r:'a range) =
      match r with
      |`Range (None,None) -> "*..*"
      |`Range (Some l,None) -> (str_fn l)^"..*"
      |`Range (None,Some u) -> "*.."^(str_fn u)
      |`Range (Some l,Some u) -> (str_fn l)^".."^(str_fn u)
      |`Lt k -> "<"^(str_fn k)
      |`Lte k -> "<="^(str_fn k)
      |`Eq k -> str_fn k
      |`Gte k -> ">="^(str_fn k)
      |`Gt k -> ">"^(str_fn k)

    type repo_field = [
      | `Name
      | `Description
      | `Readme
    ]
    let string_of_repo_field = function
      |`Name -> "name"
      |`Description -> "description"
      |`Readme -> "readme"

    type date = string

    type issue_qualifier = [
    (*| `In of repo_issue_field list TODO *)
      | `Author of string
      | `Assignee of string
      | `Mentions of string
      | `Commenter of string
      | `Involves of string
      | `Team of string
    (*| `State of repo_issue_status TODO open or closed? *)
      | `Label of string
      | `Without_label of string
    (*| `No of TODO "certain metadata" TODO *)
      | `Language of string
    (*| `Is of certain metadata TODO *)
      | `Created of date range
      | `Updated of date range
      | `Merged of date range
    (*| `Status of commit status TODO *)
    (*| `Head | `Base branch TODO *)
      | `Closed of date range
    (*| `Comments of quantity? TODO *)
      | `User of string
      | `Repo of string
      | `Project of string
    ]
    let string_of_issue_qualifier (x:issue_qualifier) =
      match x with
      | `Author a -> "author:"^a
      | `Assignee a -> "assignee:"^a
      | `Mentions a -> "mentions:"^a
      | `Commenter a -> "commenter:"^a
      | `Involves a -> "involves:"^a
      | `Team t -> "team:"^t
      | `Label l -> "label:"^l
      | `Without_label l -> "-label:"^l
      | `Language l -> "language:"^l
      | `Created r -> "created:"^(string_of_range (fun x -> x) r)
      | `Updated r -> "updated:"^(string_of_range (fun x -> x) r)
      | `Merged r -> "merged:"^(string_of_range (fun x -> x) r)
      | `Closed r -> "closed:"^(string_of_range (fun x -> x) r)
      | `User u -> "user:"^u
      | `Repo r -> "repo:"^r
      | `Project p -> "project:"^p

    type qualifier = [
      | `In of repo_field list
      | `Size of int range
      | `Stars of int range
      | `Forks of int range
      | `Fork of [ `True | `Only ]
      | `Created of date range
      | `Pushed of date range
      | `User of string
      | `Language of string
    ]
    let string_of_qualifier: qualifier -> string = function
      |`In fields ->
        "in:"^(String.concat "," (List.map string_of_repo_field fields))
      |`Size r  -> "size:" ^(string_of_range string_of_int r)
      |`Stars r -> "stars:"^(string_of_range string_of_int r)
      |`Forks r -> "forks:"^(string_of_range string_of_int r)
      |`Fork `True -> "fork:true"
      |`Fork `Only -> "fork:only"
      |`Created r -> "created:"^(string_of_range (fun x -> x) r)
      |`Pushed  r -> "pushed:" ^(string_of_range (fun x -> x) r)
      |`User u -> "user:"^u
      |`Language l -> "language:"^l
  end

  module Pull = struct
    open Lwt

    let for_repo ?token ?(state=`Open) ~user ~repo () =
      let params = Filter.([
        "state", string_of_state state;
      ]) in
      API.get_stream ?token ~params ~uri:(URI.repo_pulls ~user ~repo)
        (fun b -> return (pulls_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.pull ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (pull_of_string b))

    let create ?token ~user ~repo ~pull () =
      let uri = URI.repo_pulls ~user ~repo in
      let body = string_of_new_pull pull in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (pull_of_string b))

    let create_from_issue ?token ~user ~repo ~pull_issue () =
      let uri = URI.repo_pulls ~user ~repo in
      let body = string_of_new_pull_issue pull_issue in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (pull_of_string b))

    let update ?token ~user ~repo ~update_pull ~num () =
      let uri = URI.pull ~user ~repo ~num in
      let body = string_of_update_pull update_pull in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (pull_of_string b))

    let commits ?token ~user ~repo ~num () =
      let uri = URI.pull_commits ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (commits_of_string b))

    let files ?token ~user ~repo ~num () =
      let uri = URI.pull_files ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (files_of_string b))

    let is_merged ?token ~user ~repo ~num () =
      let uri = URI.pull_merge ~user ~repo ~num in
      let fail_handlers = [
        API.code_handler ~expected_code:`Not_found  (fun _ -> return false);
      ] in
      API.get ?token ~uri ~expected_code:`No_content ~fail_handlers
        (fun _ -> return true)

    let merge ?token ~user ~repo ~num ?merge_commit_message () =
      let uri = URI.pull_merge ~user ~repo ~num in
      let body = string_of_merge_request {merge_commit_message} in
      API.put ?token ~body ~uri ~expected_code:`OK (fun b -> return (merge_of_string b))

  end

  module Milestone = struct
    open Lwt

    let for_repo ?token ?(state=`Open) ?(sort=`Due_date) ?(direction=`Desc)
        ~user ~repo () =
      let params = Filter.([
        "direction", string_of_direction direction;
        "sort", string_of_sort sort;
        "state", string_of_state state ]) in
      API.get_stream ?token ~params ~uri:(URI.repo_milestones ~user ~repo)
        (fun b -> return (milestones_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.milestone ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (milestone_of_string b))

    let delete ?token ~user ~repo ~num () =
      let uri = URI.milestone ~user ~repo ~num in
      API.delete ?token ~uri (fun _ -> return ())

    let create ?token ~user ~repo ~milestone () =
      let uri = URI.repo_milestones ~user ~repo in
      let body = string_of_new_milestone milestone in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (milestone_of_string b))

    let update ?token ~user ~repo ~milestone ~num () =
      let uri = URI.milestone ~user ~repo ~num in
      let body = string_of_update_milestone milestone in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (milestone_of_string b))

    let labels ?token ~user ~repo ~num () =
      let uri = URI.milestone_labels ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (labels_of_string b))
  end

  module Release = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      API.get_stream ?token ~uri:(URI.repo_releases ~user ~repo)
        (fun b -> return (releases_of_string b))

    let get ?token ~user ~repo ~id () =
      let uri = URI.repo_release ~user ~repo ~id in
      API.get ?token ~uri (fun b -> return (release_of_string b))

    let get_by_tag_name ?token ~user ~repo ~tag () =
      let uri = URI.repo_tag_name ~user ~repo ~tag in
      API.get ?token ~uri (fun b -> return (release_of_string b))

    let get_latest ?token ~user ~repo () =
      let uri = URI.repo_release_latest ~user ~repo in
      API.get ?token ~uri (fun b -> return (release_of_string b))

    let create ?token ~user ~repo ~release () =
      let uri = URI.repo_releases ~user ~repo in
      let body = string_of_new_release release in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (release_of_string b))

    let delete ?token ~user ~repo ~id () =
      let uri = URI.repo_release ~user ~repo ~id in
      API.delete ?token ~uri (fun _ -> return ())

    let update ?token ~user ~repo ~release ~id () =
      let uri = URI.repo_release ~user ~repo ~id in
      let body = string_of_update_release release in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (release_of_string b))

    let upload_asset ?token ~user ~repo ~id ~filename ~content_type ~body () =
      let headers = Cohttp.Header.init_with "content-type" content_type in
      let params = ["name", filename] in
      let uri = URI.upload_release_asset ~user ~repo ~id in
      API.post ?token ~params ~headers ~body ~uri ~expected_code:`Created
        (fun _b -> return ())

    let delete_asset ?token ~user ~repo ~id () =
      let uri = URI.delete_asset ~user ~repo ~id in
      API.delete ?token ~uri (fun _ -> return ())

    let get_asset ?token ~user ~repo ~id () =
      let uri = URI.get_asset ~user ~repo ~id in
      API.get ?token ~uri (fun b -> return (release_asset_of_string b))

    let list_assets ?token ~user ~repo ~id () =
      let uri = URI.get_release_assets ~user ~repo ~id in
      API.get ?token ~uri (fun b -> return (release_assets_of_string b))
  end

  module Deploy_key = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      API.get_stream ?token ~uri:(URI.repo_deploy_keys ~user ~repo)
        (fun b -> return (deploy_keys_of_string b))

    let get ?token ~user ~repo ~id () =
      let uri = URI.repo_deploy_key ~user ~repo ~id in
      API.get ?token ~uri (fun b -> return (deploy_key_of_string b))

    let delete ?token ~user ~repo ~id () =
      let uri = URI.repo_deploy_key ~user ~repo ~id in
      API.delete ?token ~uri (fun _ -> return ())

    let create ?token ~user ~repo ~new_key () =
      let uri = URI.repo_deploy_keys ~user ~repo in
      let body = string_of_new_deploy_key new_key in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (deploy_key_of_string b))
  end

  module Issue = struct
    open Lwt

    let for_repo ?token ?creator ?mentioned ?assignee
        ?labels ?milestone ?state ?(sort=`Created)
        ?(direction=`Desc) ~user ~repo () =
      let params = Filter.([
        "direction", string_of_direction direction;
        "sort", string_of_issue_sort sort;
      ]) in
      let params = match state with
        | None -> params
        | Some s -> ("state", Filter.string_of_state s)::params
      in
      let params = match milestone with
        | None -> params
        | Some m -> ("milestone", Filter.string_of_milestone m)::params
      in
      let params = match assignee with
        | None -> params
        | Some a -> ("assignee", Filter.string_of_user a)::params
      in
      let params = match creator with
        | None -> params
        | Some c -> ("creator", c)::params
      in
      let params = match mentioned with
        | None -> params
        | Some m -> ("mentioned", m)::params
      in
      let params = match labels with
        | None -> params
        | Some l -> ("labels",String.concat "," l)::params
      in
      let uri = URI.repo_issues ~user ~repo in
      API.get_stream ?token ~params ~uri (fun b -> return (issues_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.repo_issue ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (issue_of_string b))

    let create ?token ~user ~repo ~issue () =
      let body = string_of_new_issue issue in
      let uri = URI.repo_issues ~user ~repo in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (issue_of_string b))

    let update ?token ~user ~repo ~num ~issue () =
      let body = string_of_update_issue issue in
      let uri = URI.repo_issue ~user ~repo ~num in
      API.patch ~body ?token ~uri ~expected_code:`OK
        (fun b -> return (issue_of_string b))

    let events_for_repo ?token ~user ~repo () =
      let uri = URI.repo_issues_events ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (repo_issues_events_of_string b))

    let events ?token ~user ~repo ~num () =
      let uri = URI.repo_issue_events ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (repo_issue_events_of_string b))

    let timeline_events ?token ~user ~repo ~num () =
      let uri = URI.issue_timeline ~user ~repo ~num in
      API.get_stream ?token ~uri ~media_type:"application/vnd.github.mockingbird-preview"
        (fun b -> return (timeline_events_of_string b))

    let comments ?token ?since ~user ~repo ~num () =
      let params = match since with
        | None -> []
        | Some s -> ["since", s]
      in
      let uri = URI.issue_comments ~user ~repo ~num in
      API.get_stream ?token ~params ~uri (fun b -> return (issue_comments_of_string b))

    let comments_for_repo ?token ?sort ?direction ?since ~user ~repo () =
      let params = [] in
      let params = match sort with
        | None -> params
        | Some s -> ("sort", Filter.string_of_issue_comment_sort s)::params
      in
      let params = match direction with
        | None -> params
        | Some d -> ("direction", Filter.string_of_direction d)::params
      in
      let params = match since with
        | None -> params
        | Some s -> ("since", s)::params
      in
      let uri = URI.issues_comments ~user ~repo in
      API.get_stream ?token ~params ~uri (fun b -> return (issue_comments_of_string b))

    let create_comment ?token ~user ~repo ~num ~body () =
      let body = string_of_new_issue_comment { new_issue_comment_body=body } in
      let uri = URI.issue_comments ~user ~repo ~num in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (issue_comment_of_string b))

    let get_comment ?token ~user ~repo ~id () =
      let uri = URI.issue_comment ~user ~repo ~id in
      API.get ?token ~uri (fun b -> return (issue_comment_of_string b))

    let update_comment ?token ~user ~repo ~id ~body () =
      let body = string_of_new_issue_comment { new_issue_comment_body=body } in
      let uri = URI.issue_comment ~user ~repo ~id in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (issue_comment_of_string b))

    let delete_comment ?token ~user ~repo ~id () =
      let uri = URI.issue_comment ~user ~repo ~id in
      API.delete ?token ~uri ~expected_code:`No_content (fun _ -> return ())

    let labels ?token ~user ~repo ~num () =
      let uri = URI.issue_labels ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (labels_of_string b))

    let add_labels ?token ~user ~repo ~num ~labels () =
      let body = string_of_label_names labels in
      let uri = URI.issue_labels ~user ~repo ~num in
      API.post ?token ~body ~uri ~expected_code:`OK (fun b -> return (labels_of_string b))

    let remove_label ?token ~user ~repo ~num ~name () =
      let uri = URI.issue_label ~user ~repo ~num ~name in
      API.delete ?token ~uri ~expected_code:`OK (fun b -> return (labels_of_string b))

    let replace_labels ?token ~user ~repo ~num ~labels () =
      let body = string_of_label_names labels in
      let uri = URI.issue_labels ~user ~repo ~num in
      API.put ?token ~body ~uri ~expected_code:`OK (fun b -> return (labels_of_string b))

    let remove_labels ?token ~user ~repo ~num () =
      let uri = URI.issue_labels ~user ~repo ~num in
      API.delete ?token ~uri ~expected_code:`No_content (fun _b -> return ())

    let is_issue = function
      | { issue_pull_request = None; _ } -> true
      | _ -> false

    let is_pull = function
      | { issue_pull_request = None; _ } -> false
      | _ -> true
  end

  module Label = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      let uri = URI.repo_labels ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (labels_of_string b))

    let get ?token ~user ~repo ~name () =
      let uri = URI.repo_label ~user ~repo ~name in
      API.get ?token ~uri (fun b -> return (label_of_string b))

    let create ?token ~user ~repo ~label () =
      let body = string_of_new_label label in
      let uri = URI.repo_labels ~user ~repo in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (label_of_string b))

    let update ?token ~user ~repo ~name ~label () =
      let body = string_of_new_label label in
      let uri = URI.repo_label ~user ~repo ~name in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (label_of_string b))

    let delete ?token ~user ~repo ~name () =
      let uri = URI.repo_label ~user ~repo ~name in
      API.delete ?token ~uri ~expected_code:`No_content (fun _ -> return ())
  end

  module Collaborator = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      let uri = URI.repo_collaborators ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (linked_users_of_string b))

    let exists ?token ~user ~repo ~name () =
      let uri = URI.repo_collaborator ~user ~repo ~name in
      let fail_handlers = [
        API.code_handler ~expected_code:`Not_found  (fun _ -> return false);
      ] in
      API.get ?token ~uri ~expected_code:`No_content ~fail_handlers
        (fun _ -> return true)

    let add ?token ~user ~repo ~name ?permission () =
      let params = match permission with
        | None -> None
        | Some p -> Some ["permission", Github_j.string_of_team_permission p]
      in
      let uri = URI.repo_collaborator ~user ~repo ~name in
      API.put ?token ~uri ?params ~expected_code:`No_content (fun _ -> return ())

    let remove ?token ~user ~repo ~name () =
      let uri = URI.repo_collaborator ~user ~repo ~name in
      API.delete ?token ~uri ~expected_code:`No_content (fun _ -> return ())
  end

  module Status = struct
    open Lwt

    let for_ref ?token ~user ~repo ~git_ref () =
      let uri = URI.repo_statuses ~user ~repo ~git_ref in
      API.get_stream ?token ~uri (fun b -> return (statuses_of_string b))

    let create ?token ~user ~repo ~sha ~status () =
      let uri = URI.repo_statuses ~user ~repo ~git_ref:sha in
      let body = string_of_new_status status in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (status_of_string b))

    let get ?token ~user ~repo ~sha () =
      let uri = URI.repo_commit_status ~user ~repo ~git_ref:sha in
      API.get ?token ~uri (fun b -> return (combined_status_of_string b))
  end

  module Git_obj = struct

    let type_to_string (o:obj_type)=
      match o with
      |`Tree -> "tree"
      |`Commit -> "commit"
      |`Blob -> "blob"
      |`Tag -> "tag"

    let split_ref ref =
      match Stringext.split ~max:3 ~on:'/' ref with
      |[_;ty;tl] -> ty, tl
      |_ -> "", ref
  end

  module Repo = struct
    module Hook = struct
      open Lwt

      let for_repo ?token ~user ~repo () =
        let uri = URI.repo_hooks ~user ~repo in
        API.get_stream ?token ~uri (fun b -> return (hooks_of_string b))

      let get ?token ~user ~repo ~id () =
        let uri = URI.repo_hook ~user ~repo ~id in
        API.get ?token ~uri (fun b -> return (hook_of_string b))

      let create ?token ~user ~repo ~hook () =
        let uri = URI.repo_hooks ~user ~repo in
        let body = string_of_new_hook hook in
        API.post ~body ?token ~uri ~expected_code:`Created
          (fun b -> return (hook_of_string b))

      let update ?token ~user ~repo ~id ~hook () =
        let uri = URI.repo_hook ~user ~repo ~id in
        let body = string_of_update_hook hook in
        API.patch ?token ~body ~uri ~expected_code:`OK
          (fun b -> return (hook_of_string b))

      let delete ?token ~user ~repo ~id () =
        let uri = URI.repo_hook ~user ~repo ~id in
        API.delete ?token ~uri (fun _ -> return ())
 
      let ping ?token ~user ~repo ~id () =
        let uri = URI.repo_hook_ping ~user ~repo ~id in
        API.post ?token ~uri ~expected_code:`Created (fun _ -> return ())

      let test ?token ~user ~repo ~id () =
        let uri = URI.repo_hook_test ~user ~repo ~id in
        API.post ?token ~uri ~expected_code:`No_content (fun _b -> return ())

      let parse_event = Organization.Hook.parse_event

      let parse_event_metadata = Organization.Hook.parse_event_metadata
    end

    open Lwt

    let create ?token ?organization ~repo () =
      let body = string_of_new_repo repo in
      let uri = match organization with
        | None -> URI.repos
        | Some org -> URI.org_repos ~org
      in
      API.post ~body ~expected_code:`Created ?token ~uri (fun b ->
        return (repository_of_string b)
      )

    let info ?token ~user ~repo () =
      let uri = URI.repo ~user ~repo in
      API.get ?token ~uri (fun b -> return (repository_of_string b))

    let fork ?token ?organization ~user ~repo () =
      let uri = URI.repo_forks ~user ~repo in
      let params = match organization with
        | None -> []
        | Some org -> ["organization",org]
      in
      API.post ~expected_code:`Accepted ?token ~params ~uri (fun b ->
        return (repository_of_string b)
      )

    let forks ?token ?sort ~user ~repo () =
      let uri = URI.repo_forks ~user ~repo in
      let params = match sort with
        | None -> []
        | Some sort -> ["sort",Filter.string_of_forks_sort sort]
      in
      API.get_stream ?token ~params ~uri (fun b ->
        return (repositories_of_string b)
      )

    let refs ?token ?ty ~user ~repo () =
      let uri = URI.repo_refs ~ty ~user ~repo in
      let fail_handlers = [
        API.code_handler
          ~expected_code:`Not_found
          (fun _ -> Lwt.return [])
      ] in
      API.get_stream ?token ~uri
        ~fail_handlers
        (fun b -> return (git_refs_of_string b))

    let get_ref ?token ~user ~repo ~name () =
      let uri = URI.repo_refs ~user ~ty:(Some name) ~repo in
      API.get ?token ~uri (fun b -> return (git_ref_of_string b))

    let branches ?token ~user ~repo () =
      let uri = URI.repo_branches ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (repo_branches_of_string b))

    let get_commits ?token ~user ~repo () =
      let uri = URI.repo_commits ~user ~repo in
      let fail_handlers = [
        API.code_handler
          ~expected_code:`Not_found
          (fun _ -> Lwt.return [])
      ] in
      API.get_stream ?token ~uri ~fail_handlers
        (fun b -> return (commits_of_string b))


    let get_commit ?token ~user ~repo ~sha () =
      let uri = URI.repo_commit ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> return (commit_of_string b))

    let get_tag ?token ~user ~repo ~sha () =
      let uri = URI.repo_tag ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> return (tag_of_string b))

    (* Retrieve a list of SHA hashes for tags, and obtain a
     * name and time for each tag.  If annotated, this is explicit,
     * and otherwise it uses the last commit *)
    let get_tags_and_times ?token ~user ~repo () =
      let open Monad in
      let tags = refs ?token ~ty:"tags" ~user ~repo () in
      Stream.map (fun hd ->
        let _,name = Git_obj.split_ref hd.git_ref_name in
        let sha = hd.git_ref_obj.obj_sha in
        match hd.git_ref_obj.obj_ty with
        |`Commit -> (* lightweight tag, so get commit info *)
          get_commit ?token ~user ~repo ~sha ()
          >>~ fun c ->
          return [name, c.commit_git.git_commit_author.info_date]
        |`Tag ->
          get_tag ?token ~user ~repo ~sha ()
          >>~ fun t ->
          return [name, t.tag_tagger.info_date]
        |_ -> return []
      ) tags

    let tags ?token ~user ~repo () =
      let uri = URI.repo_tags ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (repo_tags_of_string b))

    let contributors ?token ~user ~repo () =
      let uri = URI.repo_contributors ~user ~repo in
      API.get_stream ?token ~uri
        (fun b -> return (contributors_of_string b))

    let delete ?token ~user ~repo () =
      let uri = URI.repo ~user ~repo in
      API.delete ?token ~uri ~expected_code:`No_content (fun _b -> return ())
  end

  module Stats = struct
    open Lwt

    let contributors ?token ~user ~repo () =
      let uri = URI.repo_contributors_stats ~user ~repo in
      let fail_handlers = [
        API.code_handler
          ~expected_code:`Accepted
          (fun _ -> Lwt.return [])
      ] in
      API.get_stream ?token ~uri
        ~fail_handlers
        (fun b -> return (contributors_stats_of_string b))

    let yearly_commit_activity ?token ~user ~repo () =
      let uri = URI.repo_commit_activity ~user ~repo in
      let fail_handlers = [
          API.code_handler
            ~expected_code:`Accepted
            (fun _ -> Lwt.return [])
        ] in
      API.get_stream ?token ~uri
        ~fail_handlers
        (fun b -> return (commit_activities_of_string b))

    let weekly_commit_activity ?token ~user ~repo () =
      let uri = URI.repo_code_frequency_stats ~user ~repo in
      let fail_handlers = [
          API.code_handler
            ~expected_code:`Accepted
            (fun _ -> Lwt.return [])
        ] in
      API.get_stream ?token ~uri
        ~fail_handlers
        (fun b -> return (code_frequencies_of_string b))

    let weekly_commit_count ?token ~user ~repo () =
      let uri = URI.repo_participation_stats ~user ~repo in
      API.get ?token ~uri
        (fun b -> return (participation_of_string b))

    let hourly_commit_count ?token ~user ~repo () =
      let uri = URI.repo_code_hourly_stats ~user ~repo in

      let fail_handlers = [
          API.code_handler
            ~expected_code:`Accepted
            (fun _ -> Lwt.return [])
        ] in
      API.get_stream ?token ~uri
        ~fail_handlers
        (fun b -> return (punch_cards_of_string b))
  end

  module Event = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      let uri = URI.repo_events ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let public_events () =
      let uri = URI.public_events in
      API.get_stream ~uri (fun b -> return (events_of_string b))

    let for_network ?token ~user ~repo () =
      let uri = URI.network_events ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_org ?token ~org () =
      let uri = URI.org_events ~org in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_org_member ?token ~user ~org () =
      let uri = URI.org_member_events ~user ~org in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let received_by_user ?token ~user () =
      let uri = URI.received_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let received_by_user_public ?token ~user () =
      let uri = URI.public_received_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_user ?token ~user () =
      let uri = URI.user_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_user_public ?token ~user () =
      let uri = URI.public_user_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))
  end

  module Gist = struct
    open Lwt

    (* List gists https://docs.github.com/v3/gists/#list-gists
     * Parameters
     *  since : string   A timestamp in ISO 8601 format:
     *                   YYYY-MM-DDTHH:MM:SSZ. Only gists updated at
     *                   or after this time are returned. *)

    let uri_param_since uri = function
      | None -> uri
      | Some date -> Uri.add_query_param uri ("since", [date])

    (* List a user’s gists:
     * GET /users/:username/gists *)
    let for_user ?token ?since ~user () =
      let uri = URI.list_users_gists ~user in
      let uri = uri_param_since uri since in
      API.get_stream ?token ~uri (fun b -> return (gists_of_string b))

    (* List the authenticated user’s gists or if called anonymously,
     * this will return all public gists:
     * GET /gists *)
    let all ?token ?since () =
      let uri = URI.gists in
      let uri = uri_param_since uri since in
      API.get_stream ?token ~uri (fun b -> return (gists_of_string b))

    (* List all public gists:
     * GET /gists/public *)
    let all_public ?token ?since () =
      let uri = URI.list_all_public_gists in
      let uri = uri_param_since uri since in
      API.get_stream ?token ~uri (fun b -> return (gists_of_string b))

    (* List the authenticated user’s starred gists:
     * GET /gists/starred *)
    let starred ?token ?since () =
      let uri = URI.list_starred_gists in
      let uri = uri_param_since uri since in
      API.get_stream ?token ~uri (fun b -> return (gists_of_string b))

    (* Get a single gist https://docs.github.com/rest/reference/gists#get-a-gist
     * GET /gists/:id  *)
    let get ?token ~id () =
      let uri = URI.gist ~id in
      API.get ?token ~uri (fun b -> return (gist_of_string b))

    (* Create a gist https://docs.github.com/rest/reference/gists#create-a-gist
     * POST /gists
     * input
     *  files       hash      Required. Files that make up this gist.
     *  description string    A description of the gist.
     *  public      boolean   Indicates whether the gist is public. Default: false *)
    let create ?token ~gist () =
      let uri = URI.gists in
      let body = string_of_new_gist gist in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (gist_of_string b))

    (* Edit a gist https://docs.github.com/rest/reference/gists#update-a-gist
     * PATCH /gists/:id
     * input
     *  description string  A description of the gist.
     *  files       hash    Files that make up this gist.
     *  content     string  Updated file contents.
     *  filename    string  New name for this file. *)
    let update ?token ~id ~gist () =
      let uri = URI.gist ~id in
      let body = string_of_update_gist gist in
      API.patch ~body ?token ~uri ~expected_code:`OK (fun b -> return (gist_of_string b))

    (* List gist commits https://docs.github.com/rest/reference/gists#list-gist-commits
     * GET /gists/:id/commits *)
    let commits ?token ~id () =
      let uri = URI.gist_commits ~id in
      API.get_stream ?token ~uri (fun b -> return (gist_commits_of_string b))

    (* Star a gist https://docs.github.com/rest/reference/gists#star-a-gist
     * PUT /gists/:id/star *)
    let star ?token ~id () =
      let uri = URI.gist_star ~id in
      API.put ?token ~uri ~expected_code:`No_content (fun _b -> return ())

    (* Unstar a gist https://docs.github.com/rest/reference/gists#unstar-a-gist
     * DELETE /gists/:id/star *)
    let unstar ?token ~id () =
      let uri = URI.gist_star ~id in
      API.delete ?token ~uri ~expected_code:`No_content (fun _b -> return ())

    (* Check if a gist is starred https://docs.github.com/rest/reference/gists#check-if-a-gist-is-starred
     * GET /gists/:id/star
     * Response if gist is starred : 204 No Content
     * Response if gist is not starred : 404 Not Found *)

    (* Fork a gist https://docs.github.com/rest/reference/gists#fork-a-gist
     * POST /gists/:id/forks *)
    let fork ?token ~id () =
      let uri = URI.gist_forks ~id in
      API.post ?token ~uri ~expected_code:`Created (fun b -> return (gist_of_string b))

    (* List gist forks https://docs.github.com/rest/reference/gists#list-gist-forks
     * GET /gists/:id/forks *)
    let forks ?token ~id () =
      let uri = URI.gist_forks ~id in
      API.get_stream ?token ~uri (fun b -> return (gist_forks_of_string b))

    (* Delete a gist https://docs.github.com/rest/reference/gists#delete-a-gist
     * DELETE /gists/:id *)
    let delete ?token ~id () =
      let uri = URI.gist ~id in
      API.delete ?token ~uri ~expected_code:`No_content (fun _b -> return ())
  end

  module Check = struct
    open Lwt

    let check_name_param check_name uri = match check_name with
      | None -> uri
      | Some check_name -> Uri.add_query_params' uri [("check_name", check_name)]

    let app_id_param app_id uri = match app_id with
      | None -> uri
      | Some app_id -> Uri.add_query_params' uri [("app_id", app_id)]

    let status_param status uri = match status with
      | None -> uri
      | Some status -> Uri.add_query_params' uri [("status", status)]

    let create_check_run ?token ~owner ~repo ~body () =
      let uri = URI.check_run_create ~owner ~repo in
      API.post ?token ~uri ~body ~expected_code:`Created (fun b ->
        return (check_run_of_string b))
      
    let update_check_run ?token ~owner ~repo ~check_run_id ~body () =
      let uri = URI.check_run ~owner ~repo ~check_run_id in
      API.patch ?token ~uri ~body ~expected_code:`OK (fun b ->
        return (check_run_of_string b))
      
    let get_check_run ?token ~owner ~repo ~check_run_id () =
      let uri = URI.check_run ~owner ~repo ~check_run_id in
      API.get ?token ~uri (fun b -> 
          return (check_run_of_string b))

    let list_annotations ?token ~owner ~repo ~check_run_id () =
      let uri = URI.check_runs_list_annotation ~owner ~repo ~check_run_id in
      API.get ?token ~uri (fun b -> 
          return (check_run_annotations_of_string b))

    let list_check_runs ?token ~owner ~repo ~check_suite_id () =
      let uri = URI.check_runs_for_id ~owner ~repo ~check_suite_id in
      API.get ?token ~uri (fun b ->
          return (check_runs_list_of_string b))

    let list_check_runs_for_ref ?token ~owner ~repo ~sha ?check_name ?app_id ?status () =
      let uri = URI.check_runs_for_ref ~owner ~repo ~sha 
                |> check_name_param check_name
                |> app_id_param app_id
                |> status_param status
      in
      API.get ?token ~uri (fun b ->
          return (check_runs_list_of_string b))

    let create_check_suite ?token ~owner ~repo ~body () =
      let uri = URI.check_suite_create ~owner ~repo in
      API.post ?token ~uri ~body ~expected_code:`Created (fun b ->
          return (check_suite_of_string b))

    let update_preferences_for_check_suites ?token ~owner ~repo ~body () =
      let uri = URI.check_suite_preferences ~owner ~repo in
      API.patch ?token ~uri ~body ~expected_code:`OK (fun b ->
          return (check_suite_preferences_of_string b))

    let get_check_suite ?token ~owner ~repo ~check_suite_id () =
      let uri = URI.check_suite ~owner ~repo ~check_suite_id in
      API.get ?token ~uri (fun b -> 
          return (check_suite_of_string b))

    let rerequest_check_suite ?token ~owner ~repo ~check_suite_id () =
      let uri = URI.check_suite_rerequest ~owner ~repo ~check_suite_id in
      API.post ?token ~uri ~expected_code:`OK (fun _ -> return ())

    let list_check_suites_for_ref ?token ~owner ~repo ~sha () =
      let uri = URI.check_suite_list ~owner ~repo ~sha in
      API.get ?token ~uri (fun b -> 
          return (check_suite_list_of_string b))
    
  end
    
  module Search = struct
    open Lwt

    let search uri string_of_qualifier mk
        ?token ?sort ?(direction=`Desc) ~qualifiers ~keywords () =
      let qs = List.rev_map string_of_qualifier qualifiers in
      let q = String.concat " " (List.rev_append qs keywords) in
      let sort = match sort with
        | Some sort -> Some (Filter.string_of_repo_sort sort)
        | None -> None
      in
      let direction = Filter.string_of_direction direction in
      let params = [
        "q", q;
        "order",direction;
        "per_page",string_of_int 100;
      ]@(match sort with None -> [] | Some s -> ["sort",s]) in
      API.get_stream ~rate:Search ?token ~params ~uri (fun b ->
        return [mk b]
      )

    let repos =
      search
        URI.repo_search
        Filter.string_of_qualifier
        repository_search_of_string

    let issues =
      search
        URI.repo_search_issues
        Filter.string_of_issue_qualifier
        repository_issue_search_of_string
  end

  module Emoji = struct
    open Lwt

    let list ?token () =
      let uri = URI.emojis in
      API.get ?token ~uri (fun b -> return (emojis_of_string b))
  end
end
