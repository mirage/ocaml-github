(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

let user_agent = "ocaml-github" (* TODO: add version from build system *)

module Make(CL : Cohttp_lwt.Client) = struct

  let log_active =
    ref (try Unix.getenv "GITHUB_DEBUG" <> "0" with _ -> false)

  let log fmt =
    Printf.ksprintf (fun s ->
      match !log_active with
      | false -> ()
      | true  -> prerr_endline (">>> GitHub: " ^ s)) fmt

  (* Authorization Scopes *)
  module Scope = struct

    let string_of_scope (x:Github_t.scope) =
      match x with
      | `User -> "user"
      | `Public_repo -> "public_repo"
      | `Repo -> "repo"
      | `Gist -> "gist"
      | `Repo_status -> "repo_status"
      | `Delete_repo -> "delete_repo"
      | `UserEmail -> "user_email"
      | `UserFollow -> "user_follow"
      | `Notifications -> "notifications"
      | `Write_public_key -> "write:public_key"
      | `Repo_deployment -> "repo_deployment"
      | `Admin_org -> "admin:org"
      | `Read_org -> "read:org"
      | `Admin_repo_hook -> "admin:repo_hook"
      | `Admin_public_key -> "admin:public_key"
      | `Read_public_key -> "read:public_key"
      | `Write_repo_hook -> "write:repo_hook"
      | `Write_org -> "write:org"
      | `Read_repo_hook -> "read:repo_hook"

    let scope_of_string x : Github_t.scope option =
      match x with
      | "user" -> Some `User
      | "public_repo" -> Some `Public_repo
      | "repo" -> Some `Repo
      | "gist" -> Some `Gist
      | "repo_status" -> Some `Repo_status
      | "delete_repo" -> Some `Delete_repo
      | "user_email" -> Some `UserEmail
      | "user_follow" -> Some `UserFollow
      | "notifications" -> Some `Notifications
      | "write:public_key" -> Some `Write_public_key
      | "repo_deployment" -> Some `Repo_deployment
      | "admin:org" -> Some `Admin_org
      | "read:org" -> Some `Read_org
      | "admin:repo_hook" -> Some `Admin_repo_hook
      | "admin:public_key" -> Some `Admin_public_key
      | "read:public_key" -> Some `Read_public_key
      | "write:repo_hook" -> Some `Write_repo_hook
      | "write:org" -> Some `Write_org
      | "read:repo_hook" -> Some `Read_repo_hook
      | _ -> None

    let string_of_scopes scopes =
      String.concat "," (List.map string_of_scope scopes)

    let scopes_of_string s =
      let scopes = Stringext.split ~on:',' s in
      List.fold_left (fun a b ->
        match scope_of_string b with
        | None -> a
        | Some b -> b::a
      ) [] scopes

    let all = [ `User; `Public_repo; `Repo; `Gist; `Repo_status; `Delete_repo; `UserEmail; `UserFollow; `Notifications ]
  end

  module URI = struct
    let authorize ?scopes ?redirect_uri ~client_id () =
      let entry_uri = "https://github.com/login/oauth/authorize" in
      let uri = Uri.of_string entry_uri in
      let q = ["client_id", client_id ] in
      let q = match scopes with
      |Some scopes -> ("scope", Scope.string_of_scopes scopes) :: q
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

    let authorizations =
      Uri.of_string (Printf.sprintf "%s/authorizations" api)

    let authorization ~id =
      Uri.of_string (Printf.sprintf "%s/authorizations/%d" api id)

    let user ?login () =
      match login with
      |None -> Uri.of_string (Printf.sprintf "%s/user" api)
      |Some u -> Uri.of_string (Printf.sprintf "%s/users/%s" api u)

    let user_repos ~user =
      Uri.of_string (Printf.sprintf "%s/users/%s/repos" api user)

    let repo ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s" api user repo) 

    let repo_issues ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues" api user repo) 

    let repo_issue ~user ~repo ~issue_number =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d" api user repo issue_number) 

    let repo_tags ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/tags" api user repo)

    let repo_tag ~user ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/git/tags/%s" api user repo sha)

    let repo_branches ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/branches" api user repo)

    let repo_refs ?ty ~user ~repo =
      let suffix =
        match ty with
        |None -> ""
        |Some ty -> "/"^ty
      in
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/git/refs%s" api user repo suffix)

    let repo_commit ~user ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/commits/%s" api user repo sha)

    let repo_statuses ~user ~repo ~sha =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/statuses/%s" api user repo sha)

    let repo_hooks ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks" api user repo)

    let hook ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks/%d" api user repo num)

    let hook_test ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/hooks/%d/tests" api user repo num)

    let repo_pulls ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls" api user repo)

    let pull ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/pulls/%d" api user repo num)

    let pull_diff_text ~user ~repo ~num =
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

    let issue_comments ~user ~repo ~issue_number =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/%d/comments" api user repo issue_number)

    let issue_comment ~user ~repo ~comment_id =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/comments/%d" api user repo comment_id)

    let repo_releases ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases" api user repo)

    let repo_release ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/releases/%d" api user repo num)

    let upload_release_asset ~user ~repo ~id =
      Uri.of_string (
        Printf.sprintf "https://uploads.github.com/repos/%s/%s/releases/%d/assets"
          user repo id)
  
    let repo_deploy_keys ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/keys" api user repo)

    let repo_deploy_key ~user ~repo ~num =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/keys/%d" api user repo num)

    (* gists (some repetition here we could factor out) *)
    let list_users_gists ~user = 
      Uri.of_string (Printf.sprintf "%s/users/%s/gists" api user)

    let list_gists = 
      Uri.of_string (Printf.sprintf "%s/gists" api)

    let list_all_public_gists = 
      Uri.of_string (Printf.sprintf "%s/gists/public" api)

    let list_starred_gists = 
      Uri.of_string (Printf.sprintf "%s/gists/starred" api)

    let get_gist ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s" api id)

    let create_gist = 
      Uri.of_string (Printf.sprintf "%s/gists" api)

    let edit_gist ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s" api id)

    let list_gist_commits ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s/commits" api id)

    let star_gist ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s/star" api id)

    let unstar_gist ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s/star" api id)

    let is_gist_starred ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s/star" api id)

    let fork_gist ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s/forks" api id)

    let list_gist_forks ~id = 
      Uri.of_string (Printf.sprintf "%s/gists/%s/forks" api id)

    let delete_gist ~id =
      Uri.of_string (Printf.sprintf "%s/gists/%s" api id)

    let get_team ~id =
      Uri.of_string (Printf.sprintf "%s/teams/%d" api id)

    let org_teams ~org =
      Uri.of_string (Printf.sprintf "%s/orgs/%s/teams" api org)

    let team_repos ~id =
      Uri.of_string (Printf.sprintf "%s/teams/%d/repos" api id)
  end 

  module C = Cohttp
  module CLB = Cohttp_lwt_body

  module Monad = struct
    open Printf
    open Lwt

    (* Each API call results in either a valid response or
    * an HTTP error. Depending on the error status code, it may
    * be retried within the monad, or a permanent failure returned *)
    type error =
      | Generic of (CL.Response.t * CLB.t)
      | Semantic of Github_t.message
      | No_response
      | Bad_response of exn
    type request = {
      meth: C.Code.meth; uri: Uri.t;
      headers: C.Header.t; body: CLB.t;
    }

    type state = {
      user_agent: string option;
      token: string option
    }
    type 'a signal =
      | Request of request * (request -> 'a signal Lwt.t)
      | Response of 'a
      | Error of error
    and 'a t = state -> (state * 'a signal) Lwt.t

    let error_to_string = function
      | Generic (res, body) ->
        lwt body_s = CLB.to_string body in
        Lwt.return
          (sprintf "HTTP Error %s\nHeaders:\n%s\nBody:\n%s\n"
             (C.Code.string_of_status (CL.Response.status res))
             (String.concat "" (C.Header.to_lines (CL.Response.headers res)))
             body_s)
      | Semantic message ->
        Lwt.return
          (sprintf "GitHub Error %s\n%s"
             message.Github_t.message_message
             (List.fold_left
                (fun s {Github_t.error_resource; error_field; error_code} ->
                  let error_field = match error_field with
                    | None -> "\"\""
                    | Some x -> x
                  in
                  sprintf "%s> Resource type: %s\n  Field: %s\n  Code: %s\n"
                    s error_resource error_field error_code)
                "" message.Github_t.message_errors))
      | No_response -> Lwt.return "No response"
      | Bad_response exn ->
        Lwt.return (sprintf "Bad response: %s\n" (Printexc.to_string exn))

    let error err = Error err
    let response r = Response r
    let request ?token ?(params=[]) ({uri} as req) reqfn =
      let uri = Uri.add_query_params' uri begin match token with
        | None -> params
        | Some token -> ("access_token", token)::params
      end in Request ({req with uri}, reqfn)

    let add_ua hdrs ua =
      let hdrs = C.Header.prepend_user_agent hdrs (user_agent^" "^C.Header.user_agent) in
      match ua with
        | None -> hdrs
        | Some ua -> C.Header.prepend_user_agent hdrs ua

    let prepare_request state ({headers; uri} as req) =
      { req with
        headers=add_ua headers state.user_agent;
        uri=if List.mem_assoc "access_token" (Uri.query req.uri)
            then uri
            else match state.token with
              | Some token -> Uri.add_query_param' uri ("access_token",token)
              | None -> uri
      }

    let rec bind x fn = fun state -> match_lwt x state with
      | state, Request (req, reqfn) ->
          lwt r = reqfn (prepare_request state req) in
          bind (fun state -> Lwt.return (state, r)) fn state
      | state, Response r -> fn r state
      | state, ((Error _) as x) -> Lwt.return (state, x)

    let return r = fun state -> Lwt.return (state, Response r)
    let fail err = fun state -> Lwt.return (state, Error err)

    let initial_state = {user_agent=None; token=None}

    let run th = match_lwt bind th return initial_state with
      | _, Request (_,_) -> Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Error e -> Lwt.(error_to_string e >>= fun err ->
                           Printf.eprintf "%s%!" err; fail (Failure err))

    let (>>=) = bind
  end

  module API = struct

    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response (envelope,body as response) = function
      | (p, handler)::more ->
        if not (p response) then handle_response response more
        else begin
          let bad_response exn = Lwt.return (Monad.(error (Bad_response exn))) in
          try_lwt
            lwt body = CLB.to_string body in
            (* use a second try_lwt to be able to log the body in case of failure *)
            try_lwt
              lwt r = handler body in
              Lwt.return (Monad.response r)
            with exn ->
              (* XXX revert *)
              log "response body:\n%s" (Yojson.Basic.pretty_to_string (Yojson.Basic.from_string body));
              bad_response exn
          with exn -> bad_response exn
        end
      | [] ->
        match CL.Response.status envelope with
        | `Unprocessable_entity | `Gone ->
          lwt message = CLB.to_string body in
          let message = Github_j.message_of_string message in
          Lwt.return Monad.(error (Semantic message))
        | _ -> Lwt.return Monad.(error (Generic (envelope, body)))

    (* Force chunked-encoding
    * to be disabled (to satisfy Github, which returns 411 Length Required
    * to a chunked-encoding POST request). *)
    let lwt_req {Monad.uri; meth; headers; body} =
      log "Requesting %s" (Uri.to_string uri);
      CL.call ~headers ~body ~chunked:false meth uri

    let request resp_handlers req =
      lwt response = lwt_req req in
      log "Response code %s\n%!"
        (C.Code.string_of_status (CL.Response.status (fst response)));
      handle_response response resp_handlers

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      (fun (res,_) -> CL.Response.status res = expected_code), handler

    (* Convert a request body into a stream *)
    let realize_body = function None -> None | Some b -> Some (CLB.of_string b)

    (* Add the correct mime-type header *)
    let realize_headers headers = C.Header.add_opt headers "content-type" "application/json"

    let idempotent meth ?headers ?token ?params ~expected_code ~uri fn =
      fun state -> Lwt.return
        (state,
        (Monad.(request ?token ?params
                  {meth; uri; headers=realize_headers headers; body=CLB.empty})
            (request [code_handler ~expected_code fn])))

    let effectful meth ?headers ?body ?token ?params ~expected_code ~uri fn =
      let body = match body with None -> CLB.empty | Some b -> CLB.of_string b in
      fun state -> Lwt.return
        (state,
        (Monad.(request ?token ?params
                  {meth; uri; headers=realize_headers headers; body })
            (request [code_handler ~expected_code fn])))

    let get ?(expected_code=`OK) = idempotent `GET ~expected_code

    let post ~expected_code = effectful `POST ~expected_code

    let patch ~expected_code = effectful `PATCH ~expected_code

    let put ~expected_code = effectful `PUT ~expected_code

    let delete ?(expected_code=`No_content) = idempotent `DELETE ~expected_code

    let set_user_agent user_agent = fun state ->
      Monad.(Lwt.return ({state with user_agent=Some user_agent}, Response ()))

    let set_token token = fun state ->
      Monad.(Lwt.return ({state with token=Some token}, Response ()))

  end

  open Github_t
  open Github_j

  module Token = struct
    open Lwt
    type t = string

    let create ?(scopes=[`Repo]) ?(note="ocaml-github") ?note_url ?client_id ?client_secret ~user ~pass () =
      let req = { auth_req_scopes=scopes; auth_req_note=note; auth_req_note_url=note_url;
      auth_req_client_id=client_id; auth_req_client_secret=client_secret } in
      let body = string_of_auth_req req in
      let headers =
        C.Header.(add_authorization (init ()) (`Basic (user,pass)))
      in
      let uri = URI.authorizations in
      API.post ~headers ~body ~uri ~expected_code:`Created (fun body ->
        return (auth_of_string body))

    let get_all ~user ~pass () =
      let uri = URI.authorizations in
      let headers =
        C.Header.(add_authorization (init ()) (`Basic (user,pass)))
      in
      API.get ~headers ~uri ~expected_code:`OK (fun body ->
        return (auths_of_string body))

    let get ~user ~pass ~id () =
      let uri = URI.authorization id in
      let headers =
        C.Header.(add_authorization (init ()) (`Basic (user,pass)))
      in
      API.get ~headers ~uri ~expected_code:`OK (fun body ->
        return (auth_of_string body))

    let delete ~user ~pass ~id () =
      let uri = URI.authorization id in
      let headers = C.Header.(add_authorization (init ()) (`Basic (user,pass))) in
      API.delete ~headers ~uri ~expected_code:`No_content (fun body ->
        return ())

    (* Convert a code after a user oAuth into an access token that can
    * be used in subsequent requests.
    *)
    let of_code ~client_id ~client_secret ~code () =
      let uri = URI.token ~client_id ~client_secret ~code () in
      CL.post uri 
      >>= fun (res, body) ->
        lwt body = CLB.to_string body in
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

    let current_info ~token () =
      let uri = URI.user () in
      API.get ~token ~uri (fun body -> return (user_info_of_string body))

    let info ?token ~login () =
      let uri = URI.user ~login () in
      API.get ?token ~uri (fun body -> return (user_info_of_string body))

    let repos ~user ?(page=1) () =
      let uri = URI.user_repos ~user in
      let params = ["page",string_of_int page] in
      API.get ~uri ~params (fun b -> return (repos_of_string b))

  end

  module Filter = struct
    type state = [ `Open | `Closed ]
    let string_of_state (s:state) =
      match s with
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
  end

  module Pull = struct
    open Lwt

    let for_repo ?(state=`Open) ?token ?(page=1) ~user ~repo () =
      let params = Filter.([
        "state", string_of_state state;
        "page", string_of_int page ]) in
      API.get ?token ~params ~uri:(URI.repo_pulls ~user ~repo)
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

    let list_commits ?token ~user ~repo ~num () =
      let uri = URI.pull_commits ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (commits_of_string b))

    let list_files ?token ~user ~repo ~num () =
      let uri = URI.pull_files ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (files_of_string b))

    let is_merged ?token ~user ~repo ~num () =
      let uri = URI.pull_merge ~user ~repo ~num in
      fun state -> Lwt.return
        (state,
        Monad.(request ?token
                  {meth=`GET; uri; headers=API.realize_headers None; body=CLB.empty})
          API.(request [
            code_handler ~expected_code:`No_content (fun _ -> return true);
            code_handler ~expected_code:`Not_found  (fun _ -> return false);
          ]))

    let merge ?token ~user ~repo ~num ?merge_commit_message () =
      let uri = URI.pull_merge ~user ~repo ~num in
      let body = string_of_merge_request {merge_commit_message} in
      API.put ?token ~body ~uri ~expected_code:`OK (fun b -> return (merge_of_string b))

  end

  module Milestone = struct
    open Lwt

    let for_repo ?(state=`Open) ?(sort=`Due_date) ?(direction=`Desc) ?(page=1)
        ?token ~user ~repo () =
      let params = Filter.([
        "direction", string_of_direction direction;
        "page", string_of_int page;
        "sort", string_of_milestone_sort sort;
        "state", string_of_state state ]) in
      API.get ?token ~params ~uri:(URI.repo_milestones ~user ~repo) 
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
  end

  module Release = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      API.get ?token ~uri:(URI.repo_releases ~user ~repo) 
        (fun b -> return (releases_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.repo_release ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (release_of_string b))

    (** We need to download all the releases to obtain this *)
    let get_by_tag_name ?token ~user ~repo ~tag () =
      let open Monad in
      for_repo ?token ~user ~repo ()
      >>= fun rs -> begin
      try
          return (List.find (fun r -> r.release_tag_name = tag) rs)
      with Not_found ->
          let msg = Printf.sprintf "tag %s not found in repository %s/%s" tag user repo in
          fail (Semantic {Github_t.message_message=msg; message_errors=[]})
      end
  
    let delete ?token ~user ~repo ~num () =
      let uri = URI.repo_release ~user ~repo ~num in
      API.delete ?token ~uri (fun _ -> return ())

    let create ?token ~user ~repo ~release () =
      let uri = URI.repo_releases ~user ~repo in
      let body = string_of_new_release release in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (release_of_string b))

    let update ?token ~user ~repo ~release ~num () =
      let uri = URI.repo_release ~user ~repo ~num in
      let body = string_of_update_release release in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (release_of_string b))

    let upload_asset ?token ~user ~repo ~id ~filename ~content_type ~body () =
      let headers = Cohttp.Header.init_with "content-type" content_type in
      let params = ["name", filename] in
      let uri = URI.upload_release_asset ~user ~repo ~id in
      API.post ?token ~params ~headers ~body ~uri ~expected_code:`Created (fun b -> return ())
  end

  module Deploy_key = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      API.get ?token ~uri:(URI.repo_deploy_keys ~user ~repo) 
        (fun b -> return (deploy_keys_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.repo_deploy_key ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (deploy_key_of_string b))

    let delete ?token ~user ~repo ~num () =
      let uri = URI.repo_deploy_key ~user ~repo ~num in
      API.delete ?token ~uri (fun _ -> return ())

    let create ?token ~user ~repo ~new_key () =
      let uri = URI.repo_deploy_keys ~user ~repo in
      let body = string_of_new_deploy_key new_key in
      API.post ?token ~body ~uri ~expected_code:`Created (fun b -> return (deploy_key_of_string b))
  end

  module Issue = struct
    open Lwt
    
    let for_repo ?token ?creator ?mentioned ?labels
      ?(milestone=`Any) ?(state=`Open) ?(sort=`Created)
      ?(direction=`Desc) ?(page=1) ?assignee ~user ~repo () =
      let params = Filter.([
        "direction", string_of_direction direction;
        "page", string_of_int page;
        "sort", string_of_issue_sort sort;
        "state", string_of_state state;
        "milestone", string_of_milestone milestone ]) in
      let params = match assignee with |None -> params |Some a -> ("assignee", Filter.string_of_user a)::params in
      let params = match creator with |None -> params |Some c -> ("creator", c)::params in
      let params = match mentioned with |None -> params |Some m -> ("mentioned", m)::params in
      let params = match labels with |None -> params |Some l -> ("labels",String.concat "," l)::params in
      let uri = URI.repo_issues ~user ~repo in
      API.get ?token ~params ~uri (fun b -> return (issues_of_string b))

    let create ?token ~user ~repo ~issue () =
      let body = string_of_new_issue issue in
      let uri = URI.repo_issues ~user ~repo in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (issue_of_string b))

    let update ?token ~user ~repo ~issue_number ~issue () =
      let body = string_of_new_issue issue in
      let uri = URI.repo_issue ~user ~repo ~issue_number in
      API.patch ~body ?token ~uri ~expected_code:`OK (fun b -> return (issue_of_string b))

    let comments ?token ~user ~repo ~issue_number () =
      let uri = URI.issue_comments ~user ~repo ~issue_number in
      API.get ?token ~uri (fun b -> return (issue_comments_of_string b))

    let create_comment ?token ~user ~repo ~issue_number ~body () =
      let body = string_of_new_issue_comment { new_issue_comment_body=body } in
      let uri = URI.issue_comments ~user ~repo ~issue_number in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (issue_comment_of_string b))
  end

  module Status = struct
    open Lwt

    let for_sha ?token ~user ~repo ~sha () =
      let uri = URI.repo_statuses ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> return (statuses_of_string b))

    let create ?token ~user ~repo ~sha ~status () =
      let uri = URI.repo_statuses ~user ~repo ~sha in
      let body = string_of_new_status status in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (status_of_string b))
  end

  module Hook = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      let uri = URI.repo_hooks ~user ~repo in
      API.get ?token ~uri (fun b -> return (hooks_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.hook ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (hook_of_string b))

    let create ?token ~user ~repo ~hook () =
      let uri = URI.repo_hooks ~user ~repo in
      let body = string_of_new_hook hook in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (hook_of_string b))

    let update ?token ~user ~repo ~num ~hook () =
      let uri = URI.hook ~user ~repo ~num in
      let body = string_of_update_hook hook in
      API.patch ?token ~body ~uri ~expected_code:`OK (fun b -> return (hook_of_string b))

    let delete ?token ~user ~repo ~num () =
      let uri = URI.hook ~user ~repo ~num in
      API.delete ?token ~uri (fun _ -> return ())

    let test ?token ~user ~repo ~num () =
      let uri = URI.hook_test ~user ~repo ~num in
      API.post ?token ~uri ~expected_code:`No_content (fun b -> return ())
  end

  module Repo = struct
    open Lwt

    let info ?token ~user ~repo () =
      let uri = URI.repo ~user ~repo in
      API.get ?token ~uri (fun b -> return (repo_of_string b))

    let tags ?token ~user ~repo () =
      let uri = URI.repo_tags ~user ~repo in
      API.get ?token ~uri (fun b -> return (repo_tags_of_string b))

    let branches ?token ~user ~repo () =
      let uri = URI.repo_branches ~user ~repo in
      API.get ?token ~uri (fun b -> return (repo_branches_of_string b))

    let refs ?token ?ty ~user ~repo () =
      let uri = URI.repo_refs ?ty ~user ~repo in
      API.get ?token ~uri (fun b -> return (git_refs_of_string b))

    let commit ?token ~user ~repo ~sha () =
      let uri = URI.repo_commit ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> return (commit_of_string b))
  end

  module Git_obj = struct

    let obj_type_to_string (o:obj_type)=
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

  module Tag = struct
    open Monad

    let tag ?token ~user ~repo ~sha () =
      let uri = URI.repo_tag ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> Lwt.return (tag_of_string b))

    (* Retrieve a list of SHA hashes for tags, and obtain a
    * name and time for each tag.  If annotated, this is explicit,
    * and otherwise it uses the last commit *)
    let get_tags_and_times ?token ~user ~repo () =
      Repo.refs ?token ~ty:"tags" ~user ~repo () >>=
      fun tags ->
        let rec aux acc = function
          | [] -> return acc
          | hd :: tl -> begin
              let _,name = Git_obj.split_ref hd.git_ref_name in
              let sha = hd.git_ref_obj.obj_sha in
              match hd.git_ref_obj.obj_ty with
              |`Commit -> (* lightweight tag, so get commit info *)
                Repo.commit ?token ~user ~repo ~sha () >>=
                fun c ->
                  let acc = (name, c.commit_git.git_commit_author.info_date) :: acc in
                  aux acc tl
              |`Tag ->
                tag ?token ~user ~repo ~sha () >>=
                fun t ->
                  let acc = (name, t.tag_tagger.info_date) :: acc in
                  aux acc tl
              |_ -> aux acc tl
          end
        in aux [] tags
  end

  module Gist = struct
    open Lwt 

    (* List gists https://developer.github.com/v3/gists/#list-gists 
     * Parameters
     *  since : string   A timestamp in ISO 8601 format: 
     *                   YYYY-MM-DDTHH:MM:SSZ. Only gists updated at 
     *                   or after this time are returned. *)

    let uri_param_since uri= function
      | None -> uri
      | Some(date) -> Uri.add_query_param uri ("since", [date])

    (* List a user’s gists:
     * GET /users/:username/gists *)
    let list_users ?since ?token ~user () =
      let uri = URI.list_users_gists ~user in
      let uri = uri_param_since uri since in
      API.get ?token ~uri (fun b -> return (gists_of_string b))

    (* List the authenticated user’s gists or if called anonymously, 
     * this will return all public gists:
     * GET /gists *)
    let list ?since ?token () =
      let uri = URI.list_gists in
      let uri = uri_param_since uri since in
      API.get ?token ~uri (fun b -> return (gists_of_string b))

    (* List all public gists:
     * GET /gists/public *)
    let list_all_public ?since ?token () =
      let uri = URI.list_all_public_gists in
      let uri = uri_param_since uri since in
      API.get ?token ~uri (fun b -> return (gists_of_string b))

    (* List the authenticated user’s starred gists:
     * GET /gists/starred *)
    let list_starred ?since ~token () =
      let uri = URI.list_starred_gists in
      let uri = uri_param_since uri since in
      API.get ~token ~uri (fun b -> return (gists_of_string b))

    (* Get a single gist https://developer.github.com/v3/gists/#get-a-single-gist 
     * GET /gists/:id  *)
    let get ?token ~id () =
      let uri = URI.get_gist ~id in
      API.get ?token ~uri (fun b -> return (gist_of_string b))

    (* Create a gist https://developer.github.com/v3/gists/#create-a-gist
     * POST /gists 
     * input
     *  files       hash      Required. Files that make up this gist.
     *  description string    A description of the gist.
     *  public      boolean   Indicates whether the gist is public. Default: false *)
    let create ~token ~contents () =
      let uri = URI.create_gist in
      let body = string_of_gist_create contents in
      API.post ~body ~token ~uri ~expected_code:`Created (fun b -> return (gist_of_string b))

    (* Edit a gist https://developer.github.com/v3/gists/#edit-a-gist
     * PATCH /gists/:id
     * input
     *  description string  A description of the gist.
     *  files       hash    Files that make up this gist.
     *  content     string  Updated file contents.
     *  filename    string  New name for this file. *)
    let edit ~token ~id ~contents () = 
      let uri = URI.edit_gist ~id in
      let body = string_of_gist_edits contents in
      API.patch ~body ~token ~uri ~expected_code:`OK (fun b -> return (gist_of_string b))

    (* List gist commits https://developer.github.com/v3/gists/#list-gist-commits
     * GET /gists/:id/commits *)
    let commits ?token ~id () = 
      let uri = URI.list_gist_commits ~id in
      API.get ?token ~uri (fun b -> return (gist_history_list_of_string b))

    (* Star a gist https://developer.github.com/v3/gists/#star-a-gist
     * PUT /gists/:id/star
     * Note that you’ll need to set Content-Length to zero when calling 
     * out to this endpoint. For more information, see “HTTP verbs.” *)
    let star ~token ~id () = 
      let uri = URI.star_gist ~id in
      API.put ~token ~uri ~expected_code:`No_content (fun b -> return ())

    (* Unstar a gist https://developer.github.com/v3/gists/#unstar-a-gist
     * DELETE /gists/:id/star *)
    let unstar ~token ~id () = 
      let uri = URI.unstar_gist ~id in
      API.delete ~token ~uri ~expected_code:`No_content (fun b -> return ())

    (* Check if a gist is starred https://developer.github.com/v3/gists/#check-if-a-gist-is-starred
     * GET /gists/:id/star 
     * Response if gist is starred : 204 No Content
     * Response if gist is not starred : 404 Not Found *)

    (* Fork a gist https://developer.github.com/v3/gists/#fork-a-gist
     * POST /gists/:id/forks *)
    let fork ~token ~id () = 
      let uri = URI.fork_gist ~id in
      API.post ~token ~uri ~expected_code:`Created (fun b -> return (gist_of_string b))

    (* List gist forks https://developer.github.com/v3/gists/#list-gist-forks
     * GET /gists/:id/forks *)
    let list_forks ?token ~id () = 
      let uri = URI.list_gist_forks ~id in
      API.get ?token ~uri (fun b -> return (gist_forks_of_string b))

    (* Delete a gist https://developer.github.com/v3/gists/#delete-a-gist
     * DELETE /gists/:id *)
    let delete ~token ~id () = 
      let uri = URI.delete_gist ~id in
      API.delete ~token ~uri ~expected_code:`No_content (fun b -> return ())
  end

  module Organization = struct
    open Lwt

    let teams ?token ~org () =
      let uri = URI.org_teams ~org in
      API.get ?token ~uri (fun b -> return (teams_of_string b))
  end

  module Team = struct
    open Lwt

    let get ?token ~id () =
      let uri = URI.get_team ~id in
      API.get ?token ~uri (fun b -> return (team_of_string b))

    let repos ?token ~id () =
      let uri = URI.team_repos ~id in
      API.get ?token ~uri (fun b -> return (repos_of_string b))
  end
end 

