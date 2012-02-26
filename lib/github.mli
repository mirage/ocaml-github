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

module Scopes : sig
  type scope = User | Public_repo | Repo | Gist
  val scope_to_string : scope -> string
  val scope_of_string : string -> scope option
  val scopes_to_string : scope list -> string
  val scopes_of_string : string -> scope list
end

type error =
  | Generic of int * (string * string) list * string
  | Bad_response of exn
and 'a response =
  | Error of error
  | Response of 'a

val error_to_string : error -> string

val authorize : ?scopes:Scopes.scope list -> client_id:string -> unit -> unit response Lwt.t

module Monad : sig
  val bind : 'a response Lwt.t -> ('a -> 'b response Lwt.t) -> 'b response Lwt.t
  val return : 'a -> 'a response Lwt.t
  val run : 'a response Lwt.t -> 'a Lwt.t
  val (>>=) : 'a response Lwt.t -> ('a -> 'b response Lwt.t) -> 'b response Lwt.t
end

type token
val token_of_code : client_id:string -> client_secret:string -> code:string -> unit -> token response Lwt.t
val token_of_string : string -> token
val token_to_string : token -> string

val get : ?headers:Cohttpd.Client.headers -> token:string -> Uri.t ->
  (headers:Cohttpd.Client.headers -> body:string -> 'a) -> 'a response Lwt.t

module URI : sig
  val authorize : ?scopes:Scopes.scope list -> client_id:string -> unit -> Uri.t
  val token : client_id:string -> client_secret:string -> code:string -> unit -> Uri.t
end

module User : sig
  type t = {
    login : string;
    id : int;
    avatar_url : Uri.t option;
    gravatar_id : string option;
    url : Uri.t;
  }
  val of_json : Yojson.Basic.json -> t
end
module Issues : sig
  type filter = [ `Assigned | `Created | `Mentioned | `Subscribed ]
  type state = [ `Closed | `Open ]
  val to_state : Yojson.Basic.json -> [> `Closed | `Open ]
  type sort = [ `Comments | `Created | `Updated ]
  type direction = [ `Ascending | `Descending ]
  type milestone = [ `Any | `Int of int | `None ]
  type issue = {
    url : Uri.t;
    html_url : Uri.t;
    number : int;
    state : state;
    title : string;
    body : string;
    user : User.t;
    assignee : User.t option;
  }
  val of_json : Yojson.Basic.json -> issue
  val repo :
    ?milestone:[> `Any ] ->
    ?state:[> `Open ] ->
    ?mentioned:'a ->
    ?labels:'b ->
    ?sort:[> `Created ] ->
    ?direction:[> `Descending ] ->
    token:token ->
    user:string -> repo:string -> unit -> issue list response Lwt.t

  end
