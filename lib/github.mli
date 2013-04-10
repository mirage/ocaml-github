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

(* Authorization scopes; http://developer.github.com/v3/oauth/ *)
module Scope : sig
  val string_of_scope : Github_t.scope -> string
  val scope_of_string : string -> Github_t.scope option
  val string_of_scopes : Github_t.scope list -> string
  val scopes_of_string : string -> Github_t.scope list
  val all : Github_t.scope list
end

(* Access token to the API, usually obtained via a user oAuth *)
module Token : sig
  type t

  val of_code: client_id:string -> client_secret:string -> code:string -> unit -> t option Lwt.t

  val create : ?scopes:Github_t.scope list -> ?note:string -> ?note_url:string ->
    ?client_id:string -> ?client_secret:string ->
    user:string -> pass:string -> unit -> Github_t.auth Monad.t

  val get_all : user:string -> pass:string -> unit -> Github_t.auths Monad.t
  val get : user:string -> pass:string -> id:int -> unit -> Github_t.auth Monad.t

  val of_auth : Github_t.auth -> t
  val of_string : string -> t
  val to_string : t -> string
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
    ?expected_code:Cohttp.Code.status_code ->
    uri:Uri.t -> 
    (string -> 'a Lwt.t) -> 'a Monad.t

  val post : 
    ?headers:Cohttp.Header.t ->
    ?body:string ->
    ?token:Token.t ->
    expected_code:Cohttp.Code.status_code ->
    uri:Uri.t ->
    (string -> 'a Lwt.t) -> 'a Monad.t

   val delete : 
    ?headers:Cohttp.Header.t -> 
    ?token:Token.t -> 
    ?params:(string * string) list ->
    ?expected_code:Cohttp.Code.status_code ->
    uri:Uri.t -> 
    (string -> 'a Lwt.t) -> 'a Monad.t

  val patch : 
    ?headers:Cohttp.Header.t ->
    ?body:string ->
    ?token:Token.t ->
    expected_code:Cohttp.Code.status_code ->
    uri:Uri.t ->
    (string -> 'a Lwt.t) -> 'a Monad.t
end

(* Various useful URI generation functions, normally for displaying on a web-page.
 * The [authorize] function is the entry URL for your users, and the [token] URI
 * is the URI used to convert the result into a concrete access token *)
module URI : sig
  val authorizations : Uri.t
  val authorize : ?scopes:Github_t.scope list -> ?redirect_uri:Uri.t ->
    client_id:string -> unit -> Uri.t
  val issue_comments: user:string -> repo:string -> issue_number:int -> Uri.t
  val issue_comment: user:string -> repo:string -> comment_id:int -> Uri.t
  val token : client_id:string -> client_secret:string -> code:string -> unit -> Uri.t
  val repo_issues : user:string -> repo:string -> Uri.t
  val repo_issue : user:string -> repo:string -> issue_number:int ->  Uri.t
  val repo_pulls : user:string -> repo:string -> Uri.t
  val repo_milestones : user:string -> repo:string -> Uri.t
  val milestone : user:string -> repo:string -> num:int -> Uri.t
end

module User : sig
  val current_info : token:Token.t -> unit -> Github_t.user_info Monad.t
  (** Return the currently logged in user *)

  val info : ?token:Token.t -> login:string -> unit -> Github_t.user_info Monad.t
end

module Filter : sig
  type state = [ `Open | `Closed ]
  type milestone_sort = [ `Due_date | `Completeness ]
  type issue_sort = [ `Created | `Updated | `Comments ]
  type direction = [ `Asc | `Desc ]
  type milestone = [ `Any | `None | `Num of int ]
  type user = [ `Any | `None | `Login of string ]
end

module Pull : sig
  val for_repo :
    ?state:Filter.state ->
    ?token:Token.t ->
    user:string ->
    repo:string -> unit -> Github_t.pulls Monad.t

  val get :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> Github_t.pull Monad.t

  val create :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    pull:Github_t.new_pull -> unit -> Github_t.pull Monad.t

  val create_from_issue :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    pull_issue:Github_t.new_pull_issue ->
    unit -> Github_t.pull Monad.t

  val update :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    update_pull:Github_t.update_pull ->
    num:int -> unit -> Github_t.pull Monad.t

  val list_commits :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> Github_t.commits Monad.t

  val list_files :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> Github_t.files Monad.t

  val is_merged :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> bool Monad.t

  val merge :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    num:int ->
    ?merge_commit_message:string -> unit -> Github_t.merge Monad.t
end

module Milestone : sig
  val for_repo:
    ?state:Filter.state ->
    ?sort:Filter.milestone_sort ->
    ?direction:Filter.direction ->
    ?token:Token.t ->
    user:string -> repo:string -> unit -> Github_t.milestones Monad.t

  val get:
    ?token:Token.t ->
    user:string -> repo:string -> num:int -> unit -> Github_t.milestone Monad.t

  val create :
    ?token:Token.t ->
    user:string -> repo:string ->
    milestone:Github_t.new_milestone -> unit -> Github_t.milestone Monad.t

  val delete:
    ?token:Token.t ->
    user:string -> repo:string -> num:int -> unit -> unit Monad.t

  val update :
    ?token:Token.t ->
    user:string -> repo:string ->
    milestone:Github_t.update_milestone -> num:int ->
    unit -> Github_t.milestone Monad.t
end

module Issue: sig
  val for_repo :
    ?token:Token.t -> ?creator:string -> ?mentioned:string ->
    ?labels:string list -> ?milestone:Filter.milestone ->
    ?state:Filter.state -> ?sort:Filter.issue_sort ->
    ?direction:Filter.direction -> ?assignee:Filter.user ->
    user:string -> repo:string -> unit -> Github_t.issues Monad.t

  val create :
    ?token:Token.t -> user:string -> repo:string ->
    issue:Github_t.new_issue -> unit -> Github_t.issue Monad.t

  val update :
    ?token:Token.t -> user:string -> repo:string ->
    issue_number:int -> issue:Github_t.new_issue ->
    unit -> Github_t.issue Monad.t

  val comments :
    ?token:Token.t -> user:string -> repo:string ->
    issue_number:int -> unit -> Github_t.issue_comments Monad.t

  val create_comment :
    ?token:Token.t -> user:string -> repo:string ->
    issue_number:int -> body:string -> unit -> Github_t.issue_comment Monad.t
end

module Repo: sig
  val info:
    ?token:Token.t ->
    user:string -> repo:string ->
    unit -> Github_t.repo Monad.t

  val tags :
    ?token:Token.t ->
    user:string -> repo:string ->
    unit -> Github_t.repo_tags Monad.t

  val refs :
    ?token:Token.t ->
    ?ty:string -> user:string -> repo:string ->
    unit -> Github_t.git_refs Monad.t

  val commit :
    ?token:Token.t ->
    user:string -> repo:string -> sha:string ->
    unit -> Github_t.commit Monad.t

  val statuses :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    sha:string -> unit -> Github_t.statuses Monad.t

  val create_status :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    sha:string ->
    status:Github_t.new_status ->
    unit -> Github_t.status Monad.t

  val hooks :
    ?token:Token.t ->
    user:string ->
    repo:string -> unit -> Github_t.hooks Monad.t

  val hook :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> Github_t.hook Monad.t

  val create_hook :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    hook:Github_t.new_hook -> unit -> Github_t.hook Monad.t

  val update_hook :
    ?token:Token.t ->
    user:string ->
    repo:string ->
    num:int ->
    hook:Github_t.update_hook -> unit -> Github_t.hook Monad.t

  val delete_hook :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> unit Monad.t

  val test_hook :
    ?token:Token.t ->
    user:string ->
    repo:string -> num:int -> unit -> unit Monad.t
end

module Git_obj : sig
  val obj_type_to_string : Github_t.obj_type -> string

  (** Split a [ref] like "refs/tags/foo/bar" into ("tags","foo/bar") *)
  val split_ref : string -> string * string
end

module Tag : sig
  val tag :
    ?token:Token.t ->
    user:string -> repo:string -> sha:string ->
    unit -> Github_t.tag Monad.t

  val get_tags_and_times :
    ?token:Token.t ->
    user:string -> repo:string ->
    unit -> (string * string) list Monad.t
end
