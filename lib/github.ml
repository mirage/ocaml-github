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
    { uri with Uri.query=Some (Uri.(make_query q)) }

  let token ~client_id ~client_secret ~code () =
    let uri = Uri.of_string "https://github.com/login/oauth/access_token" in
    let q = [ "client_id", client_id; "code", code; "client_secret", client_secret ] in
    { uri with Uri.query=Some (Uri.(make_query q)) }

end 

open Printf
open Lwt
open Cohttpd.Client

type error =
| Generic of int * (string * string) list * string
| Bad_response of exn

type 'a response =
| Error of error
| Response of 'a

let error_to_string = function
| Generic (code, headers, body) ->
  sprintf "HTTP Error %d\n%s\n" code
  (String.concat "\n" (List.map (fun (k,v) -> sprintf "%s: %s" k v) headers))
| Bad_response exn -> sprintf "Bad response: %s\n" (Printexc.to_string exn)    

let request reqfn uri respfn =
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

let authorize ?scopes ~client_id () =
  let uri = URI.authorize ?scopes ~client_id () in
  (* Github will return a 302 usually, to the redirect_uri
   * registered in the application entry on Github *)
    request get uri (fun ~headers ~body -> ())

type token = string

let token ~client_id ~client_secret ~code () : token response Lwt.t =
  let uri = URI.token ~client_id ~client_secret ~code () in
  request post uri (fun ~headers ~body ->
    List.assoc "access_token" (Uri.parse_query body)
  )

module Issues = struct
  
  type filter = [
    | `Assigned
    | `Created
    | `Mentioned
    | `Subscribed
  ]

  type state = [
    | `Open
    | `Closed
  ]

  type sort = [
    | `Created  
    | `Updated
    | `Comments
  ]

  type direction = [
    | `Ascending
    | `Descending
  ]

end

