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
module Scopes = struct
  type scope =
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
  open Cohttpd.Client
  open Printf

  let authorize ?scopes ~client_id () =
    let entry_uri = "https://github.com/login/oauth/authorize" in
    let uri = Uri.of_string entry_uri in
    let q = ["client_id", client_id ] in
    let q = match scopes with
     |Some scopes -> ("scope", Scopes.scopes_to_string scopes) :: q
     |None -> q in
    Uri.with_query uri q

  let token ~client_id ~client_secret ~code () =
    let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
    let q = [ "client_id", client_id; "code", code; "client_secret", client_secret ] in
    Uri.with_query uri q

  let api = "https://api.github.com/"

end 

open Printf
open Lwt
open Cohttpd.Client

type error =
| Generic of int * (string * string) list * string
| Bad_response of exn
and 'a response =
| Error of error
| Response of 'a

let error_to_string = function
  | Generic (code, headers, body) ->
    sprintf "HTTP Error %d\n%s\n" code
      (String.concat "\n" (List.map (fun (k,v) -> sprintf "%s: %s" k v) headers))
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
  try_lwt 
    lwt headers, body = reqfn uri in
    let r =
      try Response(respfn ~headers ~body)
      with exn -> Error (Bad_response exn) 
    in
    return r
  with Cohttpd.Client.Http_error (code, headers, response) -> begin
    return (Error (Generic (code, headers, response)))
  end 

(* Authorization request, normally not used (a link in the HTML is
 * sufficient to redirect user to Github *)
let authorize ?scopes ~client_id () =
  let uri = URI.authorize ?scopes ~client_id () in
  (* Github will return a 302 usually, to the redirect_uri
   * registered in the application entry on Github *)
    request uri get (fun ~headers ~body -> ())

type token = string

(* Convert a code after a user oAuth into an access token that cdan
 * be used in subsequent requests.
 *)
let token_of_code ~client_id ~client_secret ~code () : token response Lwt.t =
  let uri = URI.token ~client_id ~client_secret ~code () in
  request uri post (fun ~headers ~body ->
    List.assoc "access_token" (Uri.query_of_encoded body)
  )

let token_of_string x = x
let token_to_string x = x

(* Add an authorization token onto a request URI *)
let request_with_token ~token uri =
  request (Uri.add_query_param uri ("access_token", token))

(* GET wrapper that takes a URI, adds an access token and calls the 
 * result on the callback function.  *)
let get ?headers ~token uri fn =
  request_with_token ~token uri (get ?headers) fn

open Yojson.Basic.Util

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

module User = struct
  type t = {
    login: string;
    id: int;
    avatar_url: Uri.t option;
    gravatar_id: string option;
    url: Uri.t;
  }

  let of_json j =
    { login = member "login" j |> to_string;
      id = member "id" j |> to_int;
      avatar_url = member "avatar_url" j |> to_uri_option;
      gravatar_id = member "gravatar_id" j |> to_string_option;
      url = member "url" j |> to_uri;
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
    |x -> raise (Type_error ("Issues.to_state",x))

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

  let repo ?(milestone=`Any) ?(state=`Open) ?mentioned ?labels ?(sort=`Created) ?(direction=`Descending) ~token ~user ~repo () =
    let uri = Uri.of_string (sprintf "%srepos/%s/%s/issues" URI.api user repo) in
    let params = [   ] in (* TODO *)
    get ~token uri (fun ~headers ~body ->
      convert_each of_json (Yojson.Basic.from_string body)
    )

end
