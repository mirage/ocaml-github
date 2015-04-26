(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

(** GitHub APIv3 client library *)
module type Github = sig
  exception Message of Github_t.message

  (** All API requests are bound through this monad. The [run] function
      will unpack an API response into an Lwt thread that will hold the
      ultimate response. *)
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t

    val run : 'a t -> 'a Lwt.t
    val embed : 'a Lwt.t -> 'a t
  end

  module Stream : sig
    type 'a t
    type 'a parse = string -> 'a list Lwt.t

    (** [next s] is the next element of the stream and a stream
        continuation if one exists. *)
    val next : 'a t -> ('a * 'a t) option Monad.t

    (** [map f s] is the lazy stream of [f] applied to elements of [s]
        as they are demanded. *)
    val map : ('a -> 'b list Monad.t) -> 'a t -> 'b t

    (** [find p s] is the first value in [s] satisfying [p] if one
        exists and a stream continuation. *)
    val find : ('a -> bool) -> 'a t -> ('a * 'a t) option Monad.t

    (** [iter f s] is after the application of [f] to each element of [s]. *)
    val iter : ('a -> unit Monad.t) -> 'a t -> unit Monad.t

    (** [to_list s] is a list with each element of [s]. *)
    val to_list : 'a t -> 'a list Monad.t

    (** [of_list l] is a stream with each element of [l]. *)
    val of_list : 'a list -> 'a t
  end

  (** Some results may require 2-factor authentication. [Result]
      values do not. [Auth] values contain the mode of 2FA and the
      continuation to be executed when the code is known. *)
  type 'a auth_continuation =
    | Result of 'a
    | Auth of string * (string -> 'a auth_continuation Monad.t)

  type +'a parse = string -> 'a Lwt.t
  type 'a handler =
    (Cohttp.Response.t * Cohttp_lwt_body.t -> bool) * 'a

  (** Authorization scopes; http://developer.github.com/v3/oauth/ *)
  module Scope : sig
    val string_of_scope : Github_t.scope -> string
    val scope_of_string : string -> Github_t.scope option
    val string_of_scopes : Github_t.scope list -> string
    val scopes_of_string : string -> Github_t.scope list
    val all : Github_t.scope list
  end

  (** Access token to the API, usually obtained via a user oAuth *)
  module Token : sig
    type t

    val of_code: client_id:string -> client_secret:string -> code:string -> unit -> t option Lwt.t

    val create : ?scopes:Github_t.scope list -> ?note:string ->
      ?note_url:string -> ?client_id:string -> ?client_secret:string ->
      user:string -> pass:string -> unit ->
      Github_t.auth auth_continuation Monad.t

    val get_all : user:string -> pass:string -> unit ->
      Github_t.auths auth_continuation Monad.t
    val get : user:string -> pass:string -> id:int -> unit ->
      Github_t.auth auth_continuation Monad.t
    val delete : user:string -> pass:string -> id:int -> unit ->
      unit auth_continuation Monad.t

    val of_auth : Github_t.auth -> t
    val of_string : string -> t
    val to_string : t -> string
  end

  (** Generic API accessor function, not normally used directly, but
      useful in case you wish to call an API call that isn't wrapped
      in the rest of the library (i.e. most of them at the moment!) *)
  module API : sig
    val get :
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      'a parse -> 'a Monad.t

    val get_stream :
      ?fail_handlers:'a Stream.parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a Stream.parse -> 'a Stream.t

    val post :
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      (string -> 'a Lwt.t) -> 'a Monad.t

    val delete :
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      (string -> 'a Lwt.t) -> 'a Monad.t

    val patch :
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      (string -> 'a Lwt.t) -> 'a Monad.t

    val put :
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      (string -> 'a Lwt.t) -> 'a Monad.t

    val set_user_agent : string -> unit Monad.t

    val set_token : Token.t -> unit Monad.t

    val string_of_message : Github_t.message -> string
  end

  (** Useful URI generation functions, normally for displaying on a web-page.
      The {!authorize} function is the entry URL for your users, and the {!token}
      is the URI used to convert the result into a concrete access token. *)
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
    val current_info : ?token:Token.t -> unit -> Github_t.user_info Monad.t
    (** Return the currently logged in user *)

    val info :
      ?token:Token.t -> user:string -> unit -> Github_t.user_info Monad.t

    val repositories :
      ?token:Token.t ->
      user:string -> unit -> Github_t.repository Stream.t
  end

  module Filter : sig
    type state = [ `All | `Open | `Closed ]
    type milestone_sort = [ `Due_date | `Completeness ]
    type issue_sort = [ `Created | `Updated | `Comments ]
    type repo_sort = [ `Stars | `Forks | `Updated ]
    type direction = [ `Asc | `Desc ]
    type milestone = [ `Any | `None | `Num of int ]
    type user = [ `Any | `None | `Login of string ]
    type 'a range = [
      | `Range of 'a option * 'a option
      | `Lt of 'a
      | `Lte of 'a
      | `Eq of 'a
      | `Gte of 'a
      | `Gt of 'a
    ]
    type repo_field = [
      | `Name
      | `Description
      | `Readme
    ]
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
  end

  module Pull : sig
    val for_repo :
      ?state:Filter.state ->
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.pull Stream.t

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
      repo:string -> num:int -> unit -> Github_t.commit Stream.t

    val list_files :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.file Stream.t

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
      user:string -> repo:string -> unit -> Github_t.milestone Stream.t

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

  module Release : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.release Stream.t

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> Github_t.release Monad.t

    val get_by_tag_name:
      ?token:Token.t ->
      user:string -> repo:string -> tag:string -> unit -> Github_t.release Monad.t

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.new_release -> unit -> Github_t.release Monad.t

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> unit Monad.t

    val update :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.update_release -> num:int ->
      unit -> Github_t.release Monad.t

    val upload_asset :
      ?token:Token.t ->
      user:string -> repo:string ->
      id:int -> filename:string -> content_type:string ->
      body:string ->
      unit -> unit Monad.t

  end

  module Deploy_key : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.deploy_keys Monad.t

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> Github_t.deploy_key Monad.t

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      new_key:Github_t.new_deploy_key -> unit -> Github_t.deploy_key Monad.t

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> unit Monad.t
  end

  module Issue: sig
    val for_repo :
      ?token:Token.t -> ?creator:string -> ?mentioned:string ->
      ?labels:string list -> ?milestone:Filter.milestone ->
      ?state:Filter.state -> ?sort:Filter.issue_sort ->
      ?direction:Filter.direction ->
      ?assignee:Filter.user ->
      user:string -> repo:string -> unit -> Github_t.issue Stream.t

    val create :
      ?token:Token.t -> user:string -> repo:string ->
      issue:Github_t.new_issue -> unit -> Github_t.issue Monad.t

    val update :
      ?token:Token.t -> user:string -> repo:string ->
      issue_number:int -> issue:Github_t.new_issue ->
      unit -> Github_t.issue Monad.t

    val comments :
      ?token:Token.t -> user:string -> repo:string ->
      issue_number:int -> unit -> Github_t.issue_comment Stream.t

    val create_comment :
      ?token:Token.t -> user:string -> repo:string ->
      issue_number:int -> body:string -> unit -> Github_t.issue_comment Monad.t

    val is_issue : Github_t.issue -> bool

    val is_pull : Github_t.issue -> bool
  end

  module Status : sig
    val for_sha :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      sha:string -> unit -> Github_t.statuses Monad.t

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      sha:string ->
      status:Github_t.new_status ->
      unit -> Github_t.status Monad.t
  end

  module Hook : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.hook Stream.t

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.hook Monad.t

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      hook:Github_t.new_hook -> unit -> Github_t.hook Monad.t

    val update :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      hook:Github_t.update_hook -> unit -> Github_t.hook Monad.t

    val delete :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> unit Monad.t

    val test :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> unit Monad.t
  end

  module Repo : sig
    val info:
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repository Monad.t

    val tags :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repo_tag Stream.t

    val branches :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repo_branch Stream.t

    val refs :
      ?token:Token.t ->
      ?ty:string -> user:string -> repo:string ->
      unit -> Github_t.git_ref Stream.t

    val commit :
      ?token:Token.t ->
      user:string -> repo:string -> sha:string ->
      unit -> Github_t.commit Monad.t

    val search :
      ?token:Token.t ->
      ?sort:Filter.repo_sort ->
      ?direction:Filter.direction ->
      qualifiers:Filter.qualifier list ->
      keywords:string list ->
      unit -> Github_t.repository_search Stream.t
  end

  module Event : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t

    val for_repo_issues :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t

    val public_events : unit -> Github_t.event Stream.t

    val for_network :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t

    val for_org :
      ?token:Token.t ->
      org:string -> unit -> Github_t.event Stream.t

    val for_org_member :
      ?token:Token.t ->
      user:string ->
      org:string -> unit -> Github_t.event Stream.t

    val received_by_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t

    val public_received_by_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t

    val for_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t

    val for_user_public :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
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
      unit -> (string * string) Stream.t
  end

  module Gist : sig
    val list_users : 
      ?since:string -> ?token:Token.t -> user:string -> unit ->
      Github_t.gists Monad.t
    
    val list : 
      ?since:string -> ?token:Token.t -> unit -> 
      Github_t.gists Monad.t
    
    val list_all_public : 
      ?since:string -> ?token:Token.t -> unit -> 
      Github_t.gists Monad.t
    
    val list_starred : 
      ?since:string -> token:Token.t -> unit -> 
      Github_t.gists Monad.t
    
    val get : 
      ?token:Token.t -> id:string -> unit -> 
      Github_t.gist Monad.t
    
    val create : 
      token:Token.t -> contents:Github_t.gist_create -> unit -> 
      Github_t.gist Monad.t

    val edit : 
      token:Token.t -> id:string -> 
      contents:Github_t.gist_edits -> unit -> 
      Github_t.gist Monad.t

    val commits : 
      ?token:Token.t -> id:string -> unit -> 
      Github_t.gist_history Stream.t

    val star : 
      token:Token.t -> id:string -> unit -> 
      unit Monad.t

    val unstar : 
      token:Token.t -> id:string -> unit -> 
      unit Monad.t

    (* is_starred *)

    val fork : 
      token:Token.t -> id:string -> unit -> 
      Github_t.gist Monad.t

    val list_forks : 
      ?token:Token.t -> id:string -> unit -> 
      Github_t.gist_fork Stream.t

    val delete : 
      token:Token.t -> id:string -> unit -> 
      unit Monad.t
  end

  module Organization : sig
    val teams :
      ?token:Token.t ->
      org:string ->
      unit -> Github_t.team Stream.t
  end

  module Team : sig
    val info :
      ?token:Token.t ->
      id:int ->
      unit -> Github_t.team_info Monad.t

    val repositories :
      ?token:Token.t ->
      id:int ->
      unit -> Github_t.repository Stream.t
  end

  (** [log_active] activates debug messages

      set by default when the environment variable GITHUB_DEBUG is set to 1 *)
  val log_active : bool ref

end


