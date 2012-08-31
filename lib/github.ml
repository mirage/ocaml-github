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
  type t =
  | User
  | Public_repo
  | Repo
  | Gist

  let scope_to_string = function
  | User -> "user"
  | Public_repo -> "public_repo"
  | Repo -> "repo"
  | Gist -> "gist"

  let scope_of_string = function
  | "user" -> Some User
  | "public_repo" -> Some Public_repo
  | "repo" -> Some Repo
  | "gist" -> Some Gist
  | _ -> None

  let scopes_to_string scopes =
    String.concat "," (List.map scope_to_string scopes)

  let scopes_of_string s =
    let scopes = Re_str.(split (regexp_string ",") s) in
    List.fold_left (fun a b ->
      match scope_of_string b with
      | None -> a
      | Some b -> b::a
    ) [] scopes
  
end

module URI = struct
  open Cohttp_lwt
  open Printf

  let authorize ?scopes ~client_id () =
    let entry_uri = "https://github.com/login/oauth/authorize" in
    let uri = Uri.of_string entry_uri in
    let q = ["client_id", client_id ] in
    let q = match scopes with
     |Some scopes -> ("scope", Scope.scopes_to_string scopes) :: q
     |None -> q in
    Uri.with_query uri q

  let token ~client_id ~client_secret ~code () =
    let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
    let q = [ "client_id", client_id; "code", code; "client_secret", client_secret ] in
    Uri.with_query uri q

  let api = "https://api.github.com"

  let repo_issues ~user ~repo =
     Uri.of_string (sprintf "%s/repos/%s/%s/issues" api user repo) 
end 

open Printf
open Lwt
open Cohttp_lwt

type error =
| Generic of int * (string * string) list * string
| No_response
| Bad_response of exn
and 'a response =
| Error of error
| Response of 'a

let error_to_string = function
  | Generic (code, headers, body) ->
    sprintf "HTTP Error %d\n%s\n" code
      (String.concat "\n" (List.map (fun (k,v) -> sprintf "%s: %s" k v) headers))
  | No_response -> "No response"
  | Bad_response exn -> sprintf "Bad response: %s\n" (Printexc.to_string exn)    

module Monad = struct
  type 'a t = 'a response Lwt.t

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

(* Generic request function that wraps result in 'a response *)
let request uri reqfn respfn =
  eprintf "%s\n%!" (Uri.to_string uri);
  match_lwt reqfn uri with
  |None -> return (Error No_response)
  |Some (res,body) -> begin
    try_lwt 
      lwt r = respfn ~res ~body in
      return (Response r)
    with exn -> return (Error (Bad_response exn))
  end

module C = Cohttp_lwt

(* Authorization request, normally not used (a link in the HTML is
 * sufficient to redirect user to Github *)
let authorize ?scopes ~client_id () =
  let uri = URI.authorize ?scopes ~client_id () in
  (* Github will return a 302 usually, to the redirect_uri
   * registered in the application entry on Github *)
    request uri C.Client.get (fun ~res ~body -> return ())

type token = string

(* Convert a code after a user oAuth into an access token that can
 * be used in subsequent requests.
 *)
let token_of_code ~client_id ~client_secret ~code () : token response Lwt.t =
  let uri = URI.token ~client_id ~client_secret ~code () in
  request uri C.Client.post (fun ~res ~body ->
    lwt body = C.string_of_body body in
    let t = List.assoc "access_token" (Uri.query_of_encoded body) in
    return t
  )

let token_of_string x = x
let token_to_string x = x

module API = struct

  (* Add an authorization token onto a request URI and parse the response
   * as JSON. *)
  let json_request ~token uri req resp =
    let uri = Uri.add_query_param uri ("access_token", token) in
    request uri req (fun ~res ~body ->
      lwt body = C.string_of_body body >|= Yojson.Basic.from_string in
      resp body
    )

  (* GET wrapper that takes a URI, adds an access token and calls the 
   * result on the callback function.  *)
  let get ?headers ~token ~uri fn =
    json_request ~token uri (C.Client.get ?headers) fn

  (* POST wrapper that takes a URI, adds an access token and calls the 
   * result on the callback function.  *)
  let post ?headers ?body ~token ~uri fn =
    (* Convert any body into JSON *)
    let body = match body with
     |Some x -> Cohttp_lwt.body_of_string (Yojson.Basic.to_string x)
     |None -> None in
    (* Add the correct mime-type header *)
    let headers = match headers with
     |Some x -> Cohttp.Header.add x "content-type" "application/json"
     |None -> Cohttp.Header.of_list ["content-type","application/json"] in
    json_request ~token uri (C.Client.post ~headers ?body) fn
end

(* Utility module with the Yojson parsing combinators and some extra
 * uri and option combinators *)
module Parse = struct
  include Yojson.Basic.Util

  (* Convert a JSON fragment into a URI option *)
  let to_uri_option =
    function
    | `String s -> Some (Uri.of_string s)
    | _ -> None

  (* Convert a JSON fragment into a URI *)
  let to_uri =
    function
    | `String s -> Uri.of_string s
    | x -> raise (Type_error ("to_uri_option", x))

  (* Propagate an optional value *)
  let to_option fn =
    function
    |`Null -> None
    |x -> Some (fn x)
end

(* Utility module that can be opened when creating Yojson *)
module Create = struct
  let of_string x =
    `String x

  let of_string_option =
    function
    |Some x -> `String x
    |None -> `Null

  let of_int_option =
    function
    |Some i -> `Int i
    |None -> `Null

  let of_string_list_option =
    function
    |None -> `List []
    |Some l -> `List (List.map (fun x -> `String x) l)

  let record x =
    `Assoc x
end

module User = struct
  type t = {
    login: string;
    id: int;
    avatar_url: Uri.t option;
    gravatar_id: string option;
    url: Uri.t;
  }

  let of_json j =
    let open Parse in
    let (||>) a b = member a j |> b in
    { login = "login" ||> to_string;
      id = "id" ||> to_int;
      avatar_url = "avatar_url" ||> to_uri_option;
      gravatar_id = "gravatar_id" ||> to_string_option;
      url = "url" ||> to_uri;
    }
end

module Issues = struct
  
  type filter = [
    | `Assigned
    | `Created
    | `Mentioned
    | `Subscribed ]

  type state = [
    | `Open
    | `Closed ]

  let to_state =
    function
    |`String "open" -> `Open
    |`String "closed" -> `Closed
    |x -> raise (Yojson.Basic.Util.Type_error ("Issues.to_state",x))

  type sort = [
    | `Created  
    | `Updated
    | `Comments ]

  type direction = [
    | `Ascending
    | `Descending ]

  type milestone = [
    | `Int of int
    | `None
    | `Any ]

  type issue = {
    url: Uri.t;
    html_url: Uri.t;
    number: int;
    state: state;
    title: string;
    body: string;
    user: User.t;
    assignee: User.t option;
  }

  let of_json j =
    let open Parse in
    let (||>) a b = member a j |> b in
    { url = "url" ||> to_uri; 
      html_url = "html_url" ||> to_uri;
      number = "number" ||> to_int;
      state = "state" ||> to_state;
      title = "title" ||> to_string;
      body = "body" ||> to_string;
      user = "user" ||> User.of_json;
      assignee = "assignee" ||> to_option User.of_json
    }

  let for_repo ?(milestone=`Any) ?(state=`Open) ?mentioned ?labels ?(sort=`Created) ?(direction=`Descending) ~token ~user ~repo () =
    let open Parse in
    let uri = URI.repo_issues ~user ~repo in
    let params = [   ] in (* TODO *)
    API.get ~token ~uri (fun j -> return (convert_each of_json j))

  let create ~title ?body ?assignee ?milestone ?labels ~token ~user ~repo () =
    let open Create in
    let body = record [ 
      "title", of_string title;
      "body", of_string_option body;
      "assignee", of_string_option assignee;
      "milestone", of_int_option milestone;
      "labels", of_string_list_option labels;
    ] in
    let uri = URI.repo_issues ~user ~repo in
    API.post ~body ~token ~uri (fun j -> return (of_json j))
end
