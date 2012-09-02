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

(* Authorization request, normally not used (a link in the HTML is
 * sufficient to redirect user to Github *)
val authorize : ?scopes:Github_t.scope list -> client_id:string -> unit -> unit Monad.t

(* Access token to the API, usually obtained via a user oAuth *)
module Token : sig
  type t
  val of_code: client_id:string -> client_secret:string -> code:string -> unit -> t Monad.t
  val direct : ?scopes:Github_t.scope list -> user:string -> pass:string -> unit -> t Monad.t
  val to_string : t -> string
  val of_string : string -> t
end

(* Generic API accessor function, not normally used directly, but useful in case you
 * wish to call an API call that isn't wrapped in the rest of the library (i.e. most
 * of them at the moment!)
 *)
module API : sig
  val get : 
    ?headers:Cohttp.Header.t -> 
    ?token:Token.t -> 
    ?params:(string * string) list ->
    uri:Uri.t -> 
    (string -> 'a Lwt.t) -> 'a Monad.t

  val post : 
    ?headers:Cohttp.Header.t ->
    ?body:string ->
    ?token:Token.t ->
    uri:Uri.t ->
    (string -> 'a Lwt.t) -> 'a Monad.t
end

(* Various useful URI generation functions, normally for displaying on a web-page.
 * The [authorize] function is the entry URL for your users, and the [token] URI
 * is the URI used to convert the result into a concrete access token *)
module URI : sig
  val authorizations : Uri.t
  val authorize : ?scopes:Github_t.scope list -> client_id:string -> unit -> Uri.t
  val token : client_id:string -> client_secret:string -> code:string -> unit -> Uri.t
  val repo_issues : user:string -> repo:string -> Uri.t
  val repo_milestones : user:string -> repo:string -> Uri.t
  val milestone : user:string -> repo:string -> num:int -> Uri.t
end

module Milestone : sig
  val for_repo:
    ?state:Github_t.state ->
    ?sort:Github_t.milestone_sort ->
    ?direction:Github_t.direction ->
    ?token:Token.t ->
    user:string -> repo:string -> unit -> Github_t.milestone list Monad.t

  val get:
    ?token:Token.t ->
    user:string -> repo:string -> num:int -> unit -> Github_t.milestone Monad.t
end

module Issues: sig
  val for_repo :
    ?token:Token.t ->
    user:string -> repo:string -> unit -> Github_t.issue list Monad.t

  val create :
    ?token:Token.t ->
    user:string -> repo:string -> 
    issue:Github_t.new_issue -> unit -> Github_t.issue Monad.t
end
