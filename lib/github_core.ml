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

  exception Message of Github_t.message

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

    let user ?user () =
      match user with
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

    let repo_search () =
      Uri.of_string (Printf.sprintf "%s/search/repositories" api)

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

    let repo_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/events" api user repo)

    let repo_issue_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues/events"
                       api user repo)

    let public_events = Uri.of_string (Printf.sprintf "%s/events" api)

    let network_events ~user ~repo =
      Uri.of_string (Printf.sprintf "%s/networks/%s/%s/events" api user repo)

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

    let team ~id =
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

    let string_of_message message =
      message.Github_t.message_message^
      (List.fold_left
         (fun s {Github_t.error_resource; error_field; error_code} ->
            let error_field = match error_field with
              | None -> "\"\""
              | Some x -> x
            in
            sprintf "%s\n> Resource type: %s\n  Field: %s\n  Code: %s"
              s error_resource error_field error_code)
         "" message.Github_t.message_errors)

    let error_to_string = function
      | Generic (res, body) ->
        CLB.to_string body >>= fun body_s ->
        Lwt.return
          (sprintf "HTTP Error %s\nHeaders:\n%s\nBody:\n%s\n"
             (C.Code.string_of_status (CL.Response.status res))
             (String.concat "" (C.Header.to_lines (CL.Response.headers res)))
             body_s)
      | Semantic message ->
        Lwt.return ("GitHub API error: "^string_of_message message)
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

    let rec bind x fn = fun state -> x state >>= function
      | state, Request (req, reqfn) ->
        reqfn (prepare_request state req)
        >>= fun r ->
        bind (fun state -> Lwt.return (state, r)) fn state
      | state, Response r -> fn r state
      | state, ((Error _) as x) -> Lwt.return (state, x)

    let return r = fun state -> Lwt.return (state, Response r)
    let map f m = bind m (fun x -> return (f x))

    let fail err = fun state -> Lwt.return (state, Error err)

    let initial_state = {user_agent=None; token=None}

    let run th = bind th return initial_state >>= function
      | _, Request (_,_) -> Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Error (Semantic msg) -> Lwt.(fail (Message msg))
      | _, Error e -> Lwt.(error_to_string e >>= fun err ->
                           Printf.eprintf "%s%!" err; fail (Failure err))

    let (>>=) = bind
    let (>|=) m f = map f m

    let embed lw =
      Lwt.(fun state -> lw >>= (fun v -> return (state, Response v)))
  end

  module Stream = struct
    type 'a t = {
      buffer : 'a list;
      refill : (unit -> 'a t Monad.t) option;
    }
    type 'a parse = string -> 'a list Lwt.t

    let empty = { buffer = []; refill = None }

    let rec next = Monad.(function
      | { buffer=[]; refill=None } -> return None
      | { buffer=[]; refill=Some refill } -> refill () >>= next
      | { buffer=h::buffer; refill } ->
        return (Some (h, { buffer; refill }))
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
            return { buffer; refill = Some (refill s) }
      ) in {
        buffer = [];
        refill = Some (refill s);
      }

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

    let of_list buffer = { buffer; refill=None; }
  end

  type 'a authorization =
    | Result of 'a
    | Two_factor of string

  type 'a parse = string -> 'a Lwt.t
  type 'a handler = (CL.Response.t * CLB.t -> bool) * 'a

  module API = struct
    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response (envelope,body as response) = Lwt.(function
      | (p, handler)::more ->
        if not (p response) then handle_response response more
        else begin
          let bad_response exn = return (Monad.(error (Bad_response exn))) in
          catch (fun () ->
            (* use a second try_lwt to be able to log the body in case of failure *)
            catch (fun () ->
              handler response
              >>= fun r ->
              return (Monad.response r)
            ) (fun exn ->
              (* XXX revert *)
              CLB.to_string body
              >>= fun body ->
              log "response body:\n%s" (Yojson.Basic.pretty_to_string (Yojson.Basic.from_string body));
              bad_response exn
            )
          ) bad_response
        end
      | [] ->
        match CL.Response.status envelope with
        | `Unprocessable_entity | `Gone | `Unauthorized ->
          CLB.to_string body
          >>= fun message ->
          let message = Github_j.message_of_string message in
          return Monad.(error (Semantic message))
        | _ -> return Monad.(error (Generic (envelope, body)))
    )

    (* Force chunked-encoding
    * to be disabled (to satisfy Github, which returns 411 Length Required
    * to a chunked-encoding POST request). *)
    let lwt_req {Monad.uri; meth; headers; body} =
      log "Requesting %s" (Uri.to_string uri);
      CL.call ~headers ~body ~chunked:false meth uri

    let request resp_handlers req = Lwt.(
      lwt_req req
      >>= fun response ->
      log "Response code %s\n%!"
        (C.Code.string_of_status (CL.Response.status (fst response)));
      handle_response response resp_handlers
    )

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      (fun (res,_) -> CL.Response.status res = expected_code), handler

    (* Convert a request body into a stream *)
    let realize_body = function None -> None | Some b -> Some (CLB.of_string b)

    (* Add the correct mime-type header *)
    let realize_headers headers = C.Header.add_opt headers "content-type" "application/json"

    let idempotent
        meth ?headers ?token ?params ~fail_handlers ~expected_code ~uri fn =
      fun state -> Lwt.return
        (state,
         (Monad.(request ?token ?params
                   {meth; uri; headers=realize_headers headers; body=CLB.empty})
            (request ((code_handler ~expected_code fn)::fail_handlers))))

    let just_body (_,body) = CLB.to_string body

    let effectful
        meth ?headers ?body ?token ?params ~fail_handlers ~expected_code ~uri fn =
      let body = match body with None -> CLB.empty | Some b -> CLB.of_string b in
      let fn x = Lwt.(just_body x >>= fn) in
      let fail_handlers = List.map (fun (p,fn) ->
        p,Lwt.(fun x -> just_body x >>= fn)
      ) fail_handlers in
      fun state -> Lwt.return
        (state,
        (Monad.(request ?token ?params
                  {meth; uri; headers=realize_headers headers; body })
            (request ((code_handler ~expected_code fn)::fail_handlers))))

    let map_fail_handlers f fhs = List.map (fun (p,fn) ->
      p, f fn;
    ) fhs

    let get
        ?(fail_handlers=[]) ?(expected_code=`OK) ?headers ?token ?params ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `GET ~fail_handlers ~expected_code ?headers ?token ?params ~uri
        Lwt.(fun x -> just_body x >>= fn)

    let rec next_link base = Cohttp.Link.(function
    | { context; arc = { Arc.relation; }; target }::_
      when context = Uri.of_string "" && List.mem Rel.next relation ->
      Some (Uri.resolve "" base target)
    | _::rest -> next_link base rest
    | [] -> None
    )

    let get_stream (type a)
        ?(fail_handlers:a Stream.parse handler list=[])
        ?(expected_code:Cohttp.Code.status_code=`OK)
        ?(headers:Cohttp.Header.t option) ?(token:string option) ?(params:(string * string) list option) ~(uri:Uri.t) (fn : a Stream.parse) =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x ->
          just_body x
          >>= f >>= fun buffer ->
          return { Stream.buffer; refill=None }
        ) fail_handlers
      in
      let request ~uri f =
        idempotent
          `GET ?headers ?token ?params ~fail_handlers ~expected_code ~uri f
      in
      let rec f (envelope, body) = Lwt.(
        CLB.to_string body
        >>= fun body ->
        let refill = Some (fun () ->
          let links = Cohttp.(Header.get_links envelope.Response.headers) in
          match next_link uri links with
          | None -> Monad.return Stream.empty
          | Some uri -> request ~uri f
        ) in
        fn body >>= fun buffer ->
        return { Stream.buffer; refill }
      ) in
      {
        Stream.buffer = [];
        refill = Some (fun () -> request ~uri f);
      }

    let post ?(fail_handlers=[]) ~expected_code =
      effectful `POST ~fail_handlers ~expected_code

    let patch ?(fail_handlers=[]) ~expected_code =
      effectful `PATCH ~fail_handlers ~expected_code

    let put ?(fail_handlers=[]) ~expected_code =
      effectful `PUT ~fail_handlers ~expected_code

    let delete
        ?(fail_handlers=[]) ?(expected_code=`No_content) ?headers ?token ?params
        ~uri fn =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `DELETE ~fail_handlers ~expected_code ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let set_user_agent user_agent = fun state ->
      Monad.(Lwt.return ({state with user_agent=Some user_agent}, Response ()))

    let set_token token = fun state ->
      Monad.(Lwt.return ({state with token=Some token}, Response ()))

    let string_of_message = Monad.string_of_message

  end

  open Github_t
  open Github_j

  module Token = struct
    open Lwt
    type t = string

    let two_factor_auth_handler () =
      let mode = ref "" in
      (fun (res,_) ->
         CL.Response.status res = `Unauthorized
         &&
         match C.Header.get (CL.Response.headers res) "x-github-otp" with
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
      let uri = URI.authorization id in
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
      let uri = URI.authorization id in
      let headers =
        add_otp C.Header.(add_authorization (init ()) (`Basic (user,pass))) otp
      in
      let fail_handlers = [two_factor_auth_handler ()] in
      API.delete ~headers ~uri ~fail_handlers ~expected_code:`No_content
        (fun body -> return (Result ()))

    (* Convert a code after a user oAuth into an access token that can
    * be used in subsequent requests.
    *)
    let of_code ~client_id ~client_secret ~code () =
      let uri = URI.token ~client_id ~client_secret ~code () in
      CL.post uri 
      >>= fun (res, body) ->
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

    type repo_sort = [ `Stars | `Forks | `Updated ]
    let string_of_repo_sort (s:repo_sort) =
      match s with
      |`Stars -> "stars"
      |`Forks -> "forks"
      |`Updated -> "updated"

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
    let string_of_qualifier = function
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

    let for_repo ?(state=`Open) ?token ~user ~repo () =
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

    let list_commits ?token ~user ~repo ~num () =
      let uri = URI.pull_commits ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (commits_of_string b))

    let list_files ?token ~user ~repo ~num () =
      let uri = URI.pull_files ~user ~repo ~num in
      API.get_stream ?token ~uri (fun b -> return (files_of_string b))

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

    let for_repo ?(state=`Open) ?(sort=`Due_date) ?(direction=`Desc) ?token
        ~user ~repo () =
      let params = Filter.([
        "direction", string_of_direction direction;
        "sort", string_of_milestone_sort sort;
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
  end

  module Release = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      API.get_stream ?token ~uri:(URI.repo_releases ~user ~repo)
        (fun b -> return (releases_of_string b))

    let get ?token ~user ~repo ~num () =
      let uri = URI.repo_release ~user ~repo ~num in
      API.get ?token ~uri (fun b -> return (release_of_string b))

    (** We need to stream down releases until we find the target *)
    let get_by_tag_name ?token ~user ~repo ~tag () =
      let open Monad in
      let releases = for_repo ?token ~user ~repo () in
      Stream.find (fun r -> r.release_tag_name = tag) releases
      >>= function
      | Some (r,_) -> return r
      | None ->
        let msg =
          Printf.sprintf "tag %s not found in repository %s/%s" tag user repo
        in
        fail (Semantic {Github_t.message_message=msg; message_errors=[]})

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
      ?milestone ?state ?(sort=`Created)
      ?(direction=`Desc) ?assignee ~user ~repo () =
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
      API.get_stream ?token ~uri (fun b -> return (issue_comments_of_string b))

    let create_comment ?token ~user ~repo ~issue_number ~body () =
      let body = string_of_new_issue_comment { new_issue_comment_body=body } in
      let uri = URI.issue_comments ~user ~repo ~issue_number in
      API.post ~body ?token ~uri ~expected_code:`Created (fun b -> return (issue_comment_of_string b))

    let is_issue = function { issue_pull_request = None } -> true | _ -> false

    let is_pull = function { issue_pull_request = None } -> false | _ -> true
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
      API.get_stream ?token ~uri (fun b -> return (hooks_of_string b))

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
      API.get ?token ~uri (fun b -> return (repository_of_string b))

    let tags ?token ~user ~repo () =
      let uri = URI.repo_tags ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (repo_tags_of_string b))

    let branches ?token ~user ~repo () =
      let uri = URI.repo_branches ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (repo_branches_of_string b))

    let refs ?token ?ty ~user ~repo () =
      let uri = URI.repo_refs ?ty ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (git_refs_of_string b))

    let commit ?token ~user ~repo ~sha () =
      let uri = URI.repo_commit ~user ~repo ~sha in
      API.get ?token ~uri (fun b -> return (commit_of_string b))

    let search ?token ?sort ?(direction=`Desc) ~qualifiers ~keywords () =
      let qs = List.rev_map Filter.string_of_qualifier qualifiers in
      let q = String.concat " " (List.rev_append qs keywords) in
      let sort = match sort with
        | Some sort -> Some (Filter.string_of_repo_sort sort)
        | None -> None
      in
      let direction = Filter.string_of_direction direction in
      let uri = URI.repo_search () in
      let params = [
        "q", q;
        "order",direction;
        "per_page",string_of_int 100;
      ]@(match sort with None -> [] | Some s -> ["sort",s]) in
      API.get_stream ?token ~params ~uri (fun b ->
        return [repository_search_of_string b]
      )
  end

  module Event = struct
    open Lwt

    let for_repo ?token ~user ~repo () =
      let uri = URI.repo_events ~user ~repo in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_repo_issues ?token ~user ~repo () =
      let uri = URI.repo_issue_events ~user ~repo in
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

    let public_received_by_user ?token ~user () =
      let uri = URI.public_received_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_user ?token ~user () =
      let uri = URI.user_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))

    let for_user_public ?token ~user () =
      let uri = URI.user_events ~user in
      API.get_stream ?token ~uri (fun b -> return (events_of_string b))
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
      let tags = Repo.refs ?token ~ty:"tags" ~user ~repo () in
      Stream.map (fun hd ->
        let _,name = Git_obj.split_ref hd.git_ref_name in
        let sha = hd.git_ref_obj.obj_sha in
        match hd.git_ref_obj.obj_ty with
        |`Commit -> (* lightweight tag, so get commit info *)
          Repo.commit ?token ~user ~repo ~sha ()
          >>= fun c ->
          return [name, c.commit_git.git_commit_author.info_date]
        |`Tag ->
          tag ?token ~user ~repo ~sha ()
          >>= fun t ->
          return [name, t.tag_tagger.info_date]
        |_ -> return []
      ) tags
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
      API.get_stream ?token ~uri (fun b -> return (gist_history_list_of_string b))

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
      API.get_stream ?token ~uri (fun b -> return (gist_forks_of_string b))

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
      API.get_stream ?token ~uri (fun b -> return (teams_of_string b))
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
end 

