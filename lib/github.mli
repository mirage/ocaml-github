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

(* All API requests are bound through this monad. The [run] function
 * will unpack an API response into an Lwt thread that will hold the
 * ultimate response. *)
module Monad : sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val run : 'a t -> 'a Lwt.t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(* Authorization scopes *)
module Scope : sig
  type t = User | Public_repo | Repo | Gist
end

(* Authorization request, normally not used (a link in the HTML is
 * sufficient to redirect user to Github *)
val authorize : ?scopes:Scope.t list -> client_id:string -> unit -> unit Monad.t

(* Access token to the API, usually obtained via a user oAuth *)
module Token : sig
  type t
  val of_code: client_id:string -> client_secret:string -> code:string -> unit -> t Monad.t
  val direct : ?scopes:Scope.t list -> user:string -> pass:string -> unit -> t Monad.t
  val to_string : t -> string
  val of_string : string -> t
end

(* Generic API accessor function, not normally used directly, but useful in case you
 * wish to call an API call that isn't wrapped in the rest of the library (i.e. most
 * of them at the moment!)
 *)
module API : sig
  val get : ?headers:Cohttp.Header.t ->
    ?token:Token.t -> uri:Uri.t -> (Yojson.Basic.json -> 'a Lwt.t) -> 'a Monad.t

  val post : ?headers:Cohttp.Header.t -> ?body:Yojson.Basic.json -> ?token:Token.t ->
      uri:Uri.t -> (Yojson.Basic.json -> 'a Lwt.t) -> 'a Monad.t
end

(* Various useful URI generation functions, normally for displaying on a web-page.
 * The [authorize] function is the entry URL for your users, and the [token] URI
 * is the URI used to convert the result into a concrete access token *)
module URI : sig
  val authorize : ?scopes:Scope.t list -> client_id:string -> unit -> Uri.t
  val token : client_id:string -> client_secret:string -> code:string -> unit -> Uri.t
  val repo_issues : user:string -> repo:string -> Uri.t
end

(* Github users *)
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

(* Github issues *)
module Issues : sig
  type filter = [ `Assigned | `Created | `Mentioned | `Subscribed ]
  type state = [ `Closed | `Open ]
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

  val for_repo :
    ?milestone:milestone ->
    ?state:state ->
    ?mentioned:string list ->
    ?labels:'b ->
    ?sort:sort ->
    ?direction:direction ->
    token:Token.t ->
    user:string -> repo:string -> unit -> issue list Monad.t

  val create :
    title:string ->
    ?body:string ->
    ?assignee:string ->
    ?milestone:int ->
    ?labels:string list ->
    token:Token.t ->
    user:string -> repo:string -> unit -> issue Monad.t

end
