(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
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

  (** {4 API Concepts} *)

  exception Message of Cohttp.Code.status_code * Github_t.message
  (** [Message] may be raised by any API call when the GitHub service
      returns an unexpected response code. Typical reasons for this
      exception are insufficient permissions or missing resources. *)

  (** All API requests are bound through this monad which encapsulates
      an Lwt cooperative thread and includes some state which may be
      set via {!API} functions. *)
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    (** [return x] is the value [x] wrapped in a state-sensitive Lwt thread. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind m f] is the eventual value of [f] applied to the
        contents of [m]. Its argument order is designed for currying. *)

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

  type rate = Core | Search

  (** Some results may require 2-factor authentication. [Result]
      values do not. [Two_factor] values contain the mode of 2FA. *)
  type 'a authorization =
    | Result of 'a
    | Two_factor of string

  type +'a parse = string -> 'a Lwt.t
  type 'a handler =
    (Cohttp.Response.t * Cohttp_lwt_body.t -> bool) * 'a

  (** Authorization scopes; http://developer.github.com/v3/oauth/ *)
  module Scope : sig
    val to_string : Github_t.scope -> string
    (** [to_string scope] is the string GitHub uses to indicate
        the scope constructor [scope]. *)

    val of_string : string -> Github_t.scope option
    (** [scope_of_string scope] is the constructor corresponding to
        the GitHub scope constructor [scope] if one exists. *)

    val list_to_string : Github_t.scope list -> string
    (** [string_of_scopes scopes] is the serialization for a list of
        scopes [scopes] which GitHub accepts as a set of scopes in its
        API. *)

    val list_of_string : string -> Github_t.scope list option
    (** [scopes_of_string scopes] are the scope constructors
        corresponding to the serialized list of constructors
        [scopes]. *)

    val all : Github_t.scope list
    (** [all] is a list containing every scope constructor known. *)

    val max : Github_t.scope list
    (** [max] is a list containing the mimimum scope constructors
        needed to enable full privilege. *)
  end

  (** Access token to the API, usually obtained via a user oAuth *)
  module Token : sig
    type t

    val of_code: client_id:string -> client_secret:string -> code:string -> unit -> t option Lwt.t

    val create : ?scopes:Github_t.scope list -> ?note:string ->
      ?note_url:string -> ?client_id:string -> ?client_secret:string ->
      ?fingerprint:string -> ?otp:string ->
      user:string -> pass:string -> unit ->
      Github_t.auth authorization Monad.t

    val get_all : ?otp:string -> user:string -> pass:string -> unit ->
      Github_t.auths authorization Monad.t
    val get : ?otp:string -> user:string -> pass:string -> id:int -> unit ->
      Github_t.auth option authorization Monad.t
    val delete : ?otp:string -> user:string -> pass:string -> id:int -> unit ->
      unit authorization Monad.t

    val of_auth : Github_t.auth -> t
    val of_string : string -> t
    val to_string : t -> string
  end

  (** Generic API accessor function, not normally used directly, but
      useful in case you wish to call an API call that isn't wrapped
      in the rest of the library (i.e. most of them at the moment!) *)
  module API : sig
    val code_handler : expected_code:Cohttp.Code.status_code -> 'a -> 'a handler

    val get :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      'a parse -> 'a Monad.t

    val get_stream :
      ?rate:rate ->
      ?fail_handlers:'a Stream.parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a Stream.parse -> 'a Stream.t

    val post :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      (string -> 'a Lwt.t) -> 'a Monad.t

    val delete :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      (string -> 'a Lwt.t) -> 'a Monad.t

    val patch :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      (string -> 'a Lwt.t) -> 'a Monad.t

    val put :
      ?rate:rate ->
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

    val get_rate : ?rate:rate -> ?token:Token.t -> unit -> Github_t.rate Monad.t

    val get_rate_limit : ?token:Token.t -> unit -> int Monad.t

    val get_rate_remaining : ?token:Token.t -> unit -> int Monad.t

    val get_rate_reset : ?token:Token.t -> unit -> int Monad.t

    val string_of_message : Github_t.message -> string
  end

  (** Useful URI generation functions, normally for displaying on a web-page.
      The {!authorize} function is the entry URL for your users, and the {!token}
      is the URI used to convert the result into a concrete access token. *)
  module URI : sig
    val authorizations : Uri.t
    val authorize : ?scopes:Github_t.scope list -> ?redirect_uri:Uri.t ->
      client_id:string -> state:string -> unit -> Uri.t
    (** [authorize ?scopes ?redirect_uri ~client_id ~state ()] is the
        URL to
        {{:https://developer.github.com/v3/oauth/#redirect-users-to-request-github-access}redirect
        users} to in an OAuth2 flow to create an authorization
        token. [?redirect_url] is the URL in your Web application
        where users will be sent after authorization. If omitted, it
        will default to the callback URL in GitHub's OAuth application
        settings. The [state] parameter should match the callback
        state parameter in order to protect against CSRF. *)

    val token :
      client_id:string -> client_secret:string -> code:string -> unit -> Uri.t
    (** [token ~client_id ~client_secret ~code ()] is the API endpoint
        used by {!Token.of_code} to finish the OAuth2 web flow and
        convert a temporary OAuth code into a real API access token. *)

    val repo_issues : user:string -> repo:string -> Uri.t
    (** [repo_issues ~user ~repo] is the API endpoint for all issues
        on repo [user]/[repo]. *)

    val repo_issue : user:string -> repo:string -> num:int ->  Uri.t
    (** [repo_issue ~user ~repo ~num] is the API endpoint for the
        issue [user]/[repo]#[num]. *)

    val issue_comments: user:string -> repo:string -> num:int -> Uri.t
    (** [issue_comments ~user ~repo ~num] is the API endpoint
        for the comments on issue [user]/[repo]#[num]. *)

    val issue_comment: user:string -> repo:string -> num:int -> Uri.t
    (** [issue_comment ~user ~repo ~num] is the API endpoint for
        comment [num] in repo [user]/[repo]. *)

    val repo_pulls : user:string -> repo:string -> Uri.t
    val repo_milestones : user:string -> repo:string -> Uri.t
    val milestone : user:string -> repo:string -> num:int -> Uri.t
  end

  module Rate_limit : sig
    val all : ?token:Token.t -> unit -> Github_t.rate_resources Monad.t
    val for_core : ?token:Token.t -> unit -> Github_t.rate Monad.t
    val for_search : ?token:Token.t -> unit -> Github_t.rate Monad.t
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
    (** [repo_sort] is the field by which to sort a collection of
        repositories. See {!Search.repos}. *)

    type forks_sort = [ `Newest | `Oldest | `Stargazers ]
    (** [forks_sort] is the bias used when sorting a collection of
        forks. See {!Repo.forks}. *)

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
      num:int -> filename:string -> content_type:string ->
      body:string ->
      unit -> unit Monad.t

  end

  module Deploy_key : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.deploy_key Stream.t
    (** [for_repo ~user ~repo ()] is a stream of deploy keys
        associated with repo [user]/[repo]. *)

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
      ?assignee:Filter.user ->
      ?labels:string list -> ?milestone:Filter.milestone ->
      ?state:Filter.state ->
      ?sort:Filter.issue_sort -> ?direction:Filter.direction ->
      user:string -> repo:string -> unit -> Github_t.issue Stream.t

    val create :
      ?token:Token.t -> user:string -> repo:string ->
      issue:Github_t.new_issue -> unit -> Github_t.issue Monad.t

    val update :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> issue:Github_t.new_issue ->
      unit -> Github_t.issue Monad.t

    val comments :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> unit -> Github_t.issue_comment Stream.t
    (** [comments ~user ~repo ~num ()] is a stream of issue comments
        for [user]/[repo]#[num]. *)

    val create_comment :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> body:string -> unit -> Github_t.issue_comment Monad.t
    (** [create_comment ~user ~repo ~num ~body ()] is a newly created
        issue comment on [user]/[repo]#[num] with content [body]. *)

    val is_issue : Github_t.issue -> bool

    val is_pull : Github_t.issue -> bool
  end

  module Status : sig
    val for_ref :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      git_ref:string -> unit -> Github_t.status Stream.t
    (** [for_sha ~user ~repo ~git_ref ()] is a stream of statuses
        attached to the SHA, branch name, or tag name [git_ref] in
        repo [user]/[repo]. *)

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
    val info :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repository Monad.t

    val fork :
      ?token:Token.t ->
      ?organization:string ->
      user:string -> repo:string ->
      unit -> Github_t.repository Monad.t

    val forks :
      ?token:Token.t ->
      ?sort:Filter.forks_sort ->
      user:string -> repo:string ->
      unit -> Github_t.repository Stream.t
    (** [forks ?sort ~user ~repo ()] is a stream of all repositories
        forked from [user]/[repo] sorted by [?sort] (default [`Newest]). *)

    val get_tag :
      ?token:Token.t ->
      user:string -> repo:string -> sha:string ->
      unit -> Github_t.tag Monad.t
    (** [get_tag ~user ~repo ~sha ()] is the annotated tag object with
        SHA [sha] in [user]/[repo]. *)

    val tags :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repo_tag Stream.t
    (** [tags ~user ~repo ()] is a stream of all tags in repo [user]/[repo]. *)

    val get_tags_and_times :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> (string * string) Stream.t
    (** [get_tags_and_times ~user ~repo ()] is a stream of pairs of
        tag names and creation times for all lightweight and annotated
        tags in [user]/[repo]. *)

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

  (** The [Gist] module provides access to the GitHub
      {{:https://developer.github.com/v3/gists/}gist API}. *)
  module Gist : sig
    val for_user :
      ?token:Token.t ->
      ?since:string -> user:string -> unit -> Github_t.gist Stream.t
    (** [for_user ?since ~user ()] is a stream of gists that belong to
        [user]. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. *)

    val all :
      ?token:Token.t ->
      ?since:string -> unit -> Github_t.gist Stream.t
    (** [all ?since ()] is a stream of all of the gists for the
        current token's user or all public gists if invoked without a
        current token. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. *)

    val all_public :
      ?token:Token.t ->
      ?since:string -> unit -> Github_t.gist Stream.t
    (** [all_public ?since ()] is a stream of all of the public gists
        for the current token's user or all public gists if invoked
        without a current token. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. *)

    val starred :
      ?token:Token.t ->
      ?since:string -> unit -> Github_t.gist Stream.t
    (** [starred ?since ()] is a stream of all starred gists for the
        current token's user. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. *)

    val get :
      ?token:Token.t ->
      num:string -> unit -> Github_t.gist Monad.t
    (** [get ~num ()] is the gist [num]. *)

    val create :
      ?token:Token.t ->
      gist:Github_t.new_gist -> unit -> Github_t.gist Monad.t
    (** [create ~gist ()] is a newly created gist described by [gist]. *)

    val update :
      ?token:Token.t ->
      num:string -> gist:Github_t.update_gist -> unit -> Github_t.gist Monad.t
    (** [update ~num ~gist ()] is the updated gist [num] as described
        by [gist]. *)

    val commits :
      ?token:Token.t ->
      num:string -> unit -> Github_t.gist_commit Stream.t
    (** [commits ~num ()] is a stream of commits for gist [num]. *)

    val star :
      ?token:Token.t ->
      num:string -> unit -> unit Monad.t
    (** [star ~num ()] is activated after gist [num] is marked as
        starred by the current token's user. *)

    val unstar :
      ?token:Token.t ->
      num:string -> unit -> unit Monad.t
    (** [unstar ~num ()] is activated after gist [num] is marked as
        not starred by the current token's user. *)

    (* is_starred *)

    val fork :
      ?token:Token.t ->
      num:string -> unit -> Github_t.gist Monad.t
    (** [fork ~num ()] is a newly forked gist from gist [num]. *)

    val forks :
      ?token:Token.t ->
      num:string -> unit -> Github_t.gist_fork Stream.t
    (** [forks ~num ()] is a stream of forks of gist [num]. *)

    val delete :
      ?token:Token.t ->
      num:string -> unit -> unit Monad.t
    (** [delete ~num ()] is activated after gist [num] has been deleted. *)
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
      num:int ->
      unit -> Github_t.team_info Monad.t

    val repositories :
      ?token:Token.t ->
      num:int ->
      unit -> Github_t.repository Stream.t
  end

  (** {4 Utility Modules} *)

  (** The [Git_obj] module contains utility functions for working with
      git concepts. *)
  module Git_obj : sig
    val type_to_string : Github_t.obj_type -> string
    (** [type_to_string type] is the string name of object type [type]. *)

    val split_ref : string -> string * string
    (** [split_ref ref] is the pair of ref directory and ref
        name. E.g. if [ref] is "refs/tags/foo/bar" then [split_ref
        ref] is ("tags","foo/bar"). *)
  end
end


