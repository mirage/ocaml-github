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

  let scope_of_string x : Github_t.scope option =
    match x with
    | "user" -> Some `User
    | "public_repo" -> Some `Public_repo
    | "repo" -> Some `Repo
    | "gist" -> Some `Gist
    | "repo_status" -> Some `Repo_status
    | "delete_repo" -> Some `Delete_repo
    | _ -> None

  let string_of_scopes scopes =
    String.concat "," (List.map string_of_scope scopes)

  let scopes_of_string s =
    let scopes = Re_str.(split (regexp_string ",") s) in
    List.fold_left (fun a b ->
      match scope_of_string b with
      | None -> a
      | Some b -> b::a
    ) [] scopes
end

module URI = struct
  let authorize ?scopes ~client_id () =
    let entry_uri = "https://github.com/login/oauth/authorize" in
    let uri = Uri.of_string entry_uri in
    let q = ["client_id", client_id ] in
    let q = match scopes with
     |Some scopes -> ("scope", Scope.string_of_scopes scopes) :: q
     |None -> q in
    Uri.with_query uri q

  let token ~client_id ~client_secret ~code () =
    let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
    let q = [ "client_id", client_id; "code", code; "client_secret", client_secret ] in
    Uri.with_query uri q

  let api = "https://api.github.com"

  let repo_issues ~user ~repo =
    Uri.of_string (Printf.sprintf "%s/repos/%s/%s/issues" api user repo) 

  let authorizations =
    Uri.of_string "https://api.github.com/authorizations"

  let repo_milestones ~user ~repo =
    Uri.of_string (Printf.sprintf "%s/repos/%s/%s/milestones" api user repo)

  let milestone ~user ~repo ~num =
    Uri.of_string (Printf.sprintf "%s/repos/%s/%s/milestones/%d" api user repo num)
end 

module Monad = struct
  open Lwt
  open Printf

  (* Each API call results in either a valid response or
   * an HTTP error. Depending on the error status code, it may
   * be retried within the monad, or a permanent failure returned *)
  type error =
  | Generic of int * (string * string) list * string
  | No_response
  | Bad_response of exn
  and 'a response =
  | Error of error
  | Response of 'a
  and 'a t = 'a response Lwt.t

  let error_to_string = function
    | Generic (code, headers, body) ->
      sprintf "HTTP Error %d\n%s\n" code
        (String.concat "\n" (List.map (fun (k,v) -> sprintf "%s: %s" k v) headers))
    | No_response -> "No response"
    | Bad_response exn -> sprintf "Bad response: %s\n" (Printexc.to_string exn)    

  let bind x fn =
    match_lwt x with
    |Error e -> return (Error e)
    |Response r -> fn r

  let return r =
    return (Response r)

  let run th =
    match_lwt th with
    |Response r -> Lwt.return r
    |Error e -> fail (Failure (error_to_string e))

  let (>>=) = bind
end

module C = Cohttp
module CL = Cohttp_lwt

module API = struct
  open Lwt

  (* Generic request function that wraps result in 'a response *)
  let request uri reqfn respfn =
    Printf.eprintf "%s\n%!" (Uri.to_string uri);
    match_lwt reqfn uri with
    |None ->
      return (Monad.(Error No_response))
    |Some (res,body) -> begin
      Printf.eprintf "Github response code %s\n%!" (C.Code.string_of_status (CL.Response.status res));
      (* TODO check that this is a valid response code *)
      try_lwt 
        lwt body = CL.string_of_body body in
        lwt r = respfn ~res ~body in
        return (Monad.Response r)
      with exn -> return (Monad.(Error (Bad_response exn)))
    end

  (* Add an authorization token onto a request URI and parse the response
   * as JSON. *)
  let request_with_token ?headers ?token uri req resp =
    (* Add the correct mime-type header *)
    let headers = match headers with
     |Some x -> Some (C.Header.add x "content-type" "application/json")
     |None -> Some (C.Header.of_list ["content-type","application/json"]) in
    let uri = match token with
     |Some token -> Uri.add_query_param uri ("access_token", token) 
     |None -> uri in
    request uri (req ?headers) (fun ~res ~body -> resp body)

  let get ?headers ?token ?(params=[]) ~uri fn =
    let uri = Uri.add_query_params uri params in
    request_with_token ?headers ?token uri CL.Client.get fn

  let post ?headers ?body ?token ~uri fn =
    let body = match body with |None -> None |Some b -> CL.body_of_string b in
    let chunked = false in
    request_with_token ?headers ?token uri (CL.Client.post ?body ~chunked) fn

  let patch ?headers ?body ?token ~uri fn =
    let body = match body with |None -> None |Some b -> CL.body_of_string b in
    let chunked = false in
    request_with_token ?headers ?token uri (CL.Client.patch ?body ~chunked) fn

  let delete ?headers ?token ?(params=[]) ~uri fn =
    let uri = Uri.add_query_params uri params in
    request_with_token ?headers ?token uri CL.Client.delete fn
end

(* Authorization request, normally not used (a link in the HTML is
 * sufficient to redirect user to Github *)
let authorize ?scopes ~client_id () =
  let uri = URI.authorize ?scopes ~client_id () in
  (* Github will return a 302 usually, to the redirect_uri
   * registered in the application entry on Github *)
    API.request uri CL.Client.get (fun ~res ~body -> Lwt.return_unit)

open Github_t
open Github_j
open Lwt

module Token = struct
  type t = string

  let direct ?(scopes=[`Repo]) ~user ~pass () =
    let req = { auth_req_scopes=scopes; auth_req_note="ocaml-github" } in
    let body = string_of_authorization_request req in
    let headers = C.Header.(add_authorization (init ()) (C.Auth.Basic (user,pass))) in
    API.post ~headers ~body ~uri:URI.authorizations (fun body ->
      let json = authorization_response_of_string body in
      return json.token
    )

  (* Convert a code after a user oAuth into an access token that can
   * be used in subsequent requests.
   *)
  let of_code ~client_id ~client_secret ~code () =
    let uri = URI.token ~client_id ~client_secret ~code () in
    API.request uri CL.Client.post (fun ~res ~body ->
      try
        return (Some (List.assoc "access_token" (Uri.query_of_encoded body)))
      with Not_found ->
        return None
    )

  let of_string x = x
  let to_string x = x
end
 
module Milestone = struct

  let for_repo ?(state=`Open) ?(sort=`Due_date) ?(direction=`Desc) ?token ~user ~repo () =
    (* TODO see if atdgen can generate these conversion functions to normal OCaml
     * strings. The Github_j will put quotes around the string. *)
    let string_of_state = function |`Open -> "open" |`Closed -> "closed" in
    let string_of_sort = function |`Due_date -> "due_date" |`Completeness -> "completeness" in
    let string_of_direction = function |`Asc -> "asc" |`Desc -> "desc" in
    let params = [ 
      "state", string_of_state state;
      "sort", string_of_sort sort;
      "direction", string_of_direction direction 
    ] in
    API.get ?token ~params ~uri:(URI.repo_milestones ~user ~repo) 
      (fun b -> return (milestones_of_string b))

  let get ?token ~user ~repo ~num () =
    let uri = URI.milestone ~user ~repo ~num in
    (* TODO success is 200 *)
    API.get ?token ~uri (fun b -> return (milestone_of_string b))

  let delete ?token ~user ~repo ~num () =
    let uri = URI.milestone ~user ~repo ~num in
    (* TODO success is 204 no content *)
    API.delete ?token ~uri (fun _ -> return ())

  let create ?token ~user ~repo ~milestone () =
    let uri = URI.repo_milestones ~user ~repo in
    let body = string_of_new_milestone milestone in
    (* TODO success is 201 created *)
    API.post ?token ~body ~uri (fun b -> return (milestone_of_string b))

  let update ?token ~user ~repo ~milestone ~num () =
    let uri = URI.milestone ~user ~repo ~num in
    let body = string_of_update_milestone milestone in
    (* TODO success is 200 ok *)
    API.patch ?token ~body ~uri (fun b -> return (milestone_of_string b))
end

module Issues = struct
  
  let for_repo ?token ~user ~repo () =
    let uri = URI.repo_issues ~user ~repo in
    API.get ?token ~uri (fun b -> return (issues_of_string b))

  let create ?token ~user ~repo ~issue () =
    let body = Github_j.string_of_new_issue issue in
    let uri = URI.repo_issues ~user ~repo in
    API.post ~body ?token ~uri (fun b -> return (issue_of_string b))
end
