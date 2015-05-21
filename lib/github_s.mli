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

(**
   {3 {{:https://developer.github.com/v3/}GitHub APIv3} client library}

   This library offers thin but natural bindings to GitHub's developer API.
*)

(** Modules of this type are returned from the {!Github_core.Make}
    functor which may be applied to Cohttp_lwt client libraries in
    order to run on Unix, in a browser in Javascript, or as a MirageOS
    unikernel. *)
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
    (** ['a t] is an Lwt thread sensitive to GitHub API state. *)

    val return : 'a -> 'a t
    (** [return x] is the value [x] wrapped in a state-sensitive Lwt thread. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind m f] is the eventual value of [f] applied to the
        contents of [m]. Its argument order is designed for currying. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] is {!bind} [m (fun x -> return (f x))]. Its argument
        order is designed for currying. *)

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** [m >>= f] is {!bind} [f m]. *)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** [m >|= f] is {!map} [f m]. *)

    val run : 'a t -> 'a Lwt.t
    (** [run m] is the Lwt thread corresponding to the sequence of API
        actions represented by [m]. Once a {!t} has been [run], any
        GitHub API state such as associated default security tokens or
        declared user agent string will not be available in
        subsequently bound functions. *)

    val embed : 'a Lwt.t -> 'a t
    (** [embed lwt] is an Lwt thread lifted into the GitHub API
        monad. Its monadic state will be inherited from any monadic
        values bound before it. *)
  end

  (** The [Stream] module provides an abstraction to GitHub's paginated
      endpoints. Stream creation does not initiate any network
      activity. When requests are made, results are buffered
      internally. Streams are not mutable. *)
  module Stream : sig
    type 'a t
    (** ['a t] is a stream consisting roughly of a buffer and a means
        to refill it. *)

    type 'a parse = string -> 'a list Lwt.t
    (** ['a parse] is the type of functions which extract elements
        from a paginated response. *)

    val next : 'a t -> ('a * 'a t) option Monad.t
    (** [next s] is the next element of the stream and a stream
        continuation if one exists. The input stream is not
        modified. This function offers an efficient, lazy, and uniform
        means to iterate over ordered API results which may be too
        numerous to fit into a single request/response pair. *)

    val map : ('a -> 'b list Monad.t) -> 'a t -> 'b t
    (** [map f s] is the lazy stream of [f] applied to elements of [s]
        as they are demanded. *)

    val find : ('a -> bool) -> 'a t -> ('a * 'a t) option Monad.t
    (** [find p s] is the first value in [s] satisfying [p] if one
        exists and a stream continuation for further ingestion. *)

    val iter : ('a -> unit Monad.t) -> 'a t -> unit Monad.t
    (** [iter f s] is activated after the application of [f] to each
        element of [s]. *)

    val to_list : 'a t -> 'a list Monad.t
    (** [to_list s] is a list with each element of [s]. {b Warning:}
        this function may result in {i many} successive API transactions. *)

    val of_list : 'a list -> 'a t
    (** [of_list l] is a stream with each element of [l].
        Occasionally, it is useful to write interfaces which operate
        generically on streams. [of_list] allows you to use list
        values with those interfaces. *)

    val poll : 'a t -> 'a t option Monad.t
    (** [poll stream] is a stream with items newer than [stream]'s
        items and will not resolve until any timeouts indicated by
        GitHub have elapsed. By default, GitHub throttles polling
        requests to once per minute per URL endpoint. *)
  end

  type rate = Core | Search (**)
  (** [rate] is a type used to indicate which
      {{:https://developer.github.com/v3/#rate-limiting}rate-limiting
      regime} is to be used for query quota accounting. [rate] is used
      by the function in {!API}. *)

  type 'a authorization =
    | Result of 'a
    | Two_factor of string (**)
  (** Some results may require 2-factor authentication. [Result]
      values do not. [Two_factor] values contain the mode by which a
      2FA code will be delivered. This code is required as [?otp] to
      a subsequent invocation of the function which returns this
      type. *)

  type +'a parse = string -> 'a Lwt.t
  (** ['a parse] is the type of functions which extract meaningful
      values from GitHub responses. *)

  type 'a handler =
    (Cohttp.Response.t * Cohttp_lwt_body.t -> bool) * 'a
  (** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). *)

  val log_active : bool ref
  (** [log_active] regulates debug messages. It is [true] by default
      when the environment variable [GITHUB_DEBUG] is set to 1. *)

  (** The [Scope] module abstracts GitHub's
      {{:https://developer.github.com/v3/oauth/#scopes}authorization
      scopes}. *)
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

  (** The [Token] module manipulates authorization tokens. GitHub has
      two types of tokens:
      {{:https://developer.github.com/v3/oauth/}OAuth application
      tokens} and
      {{:https://help.github.com/articles/creating-an-access-token-for-command-line-use/}"personal
      tokens"}.
      @see <https://developer.github.com/v3/oauth_authorizations/> OAuth Authorizations API
  *)
  module Token : sig
    type t
    (** [t] is the abstract type of a token. *)

    val of_code :
      client_id:string -> client_secret:string -> code:string ->
      unit -> t option Lwt.t
    (** [of_code ~client_id ~client_secret ~code ()] is the {!t}
        granted by a [code] from an
        {{:https://developer.github.com/v3/oauth/#github-redirects-back-to-your-site}OAuth
        web flow redirect}. *)

    val create : ?scopes:Github_t.scope list -> ?note:string ->
      ?note_url:string -> ?client_id:string -> ?client_secret:string ->
      ?fingerprint:string -> ?otp:string ->
      user:string -> pass:string -> unit ->
      Github_t.auth authorization Monad.t
    (** [create ?otp ~user ~pass ()] is a new authorization with the
        provided fields. When a user has enabled two-factor
        authentication, the return value will be a {!const:Two_factor}
        constructor with the one-time password delivery
        mode. Including a valid [?otp] will yield a {!const:Result} return
        value. *)

    val get_all : ?otp:string -> user:string -> pass:string -> unit ->
      Github_t.auths authorization Monad.t
    (** [get_all ~user ~pass ()] are all of the authorizations that
        this user has made. See {!create} for an explanation of how
        two-factor authentication is handled. *)

    val get : ?otp:string -> user:string -> pass:string -> id:int -> unit ->
      Github_t.auth option authorization Monad.t
    (** [get ~user ~pass ~id ()] is the authorization with identifier
        [id]. See {!create} for an explanation of how two-factor
        authentication is handled. *)

    val delete : ?otp:string -> user:string -> pass:string -> id:int -> unit ->
      unit authorization Monad.t
    (** [delete ~user ~pass ~id ()] is [Result ()] after the
        authorization with identifier [id] has been removed. See
        {!create} for an explanation of how two-factor authentication
        is handled. *)

    val of_auth : Github_t.auth -> t
    (** [of_auth auth] is the OAuth application or personal token
        contained within [auth]. *)

    val of_string : string -> t
    (** [of_string token_string] is the abstract token value
        corresponding to the string [token_string]. *)

    val to_string : t -> string
    (** [to_string token] is the string serialization of [token]. *)
  end

  (** The [API] module contains functionality that relates to the
      entirety of the GitHub API and these bindings. In particular,
      this module contains:

      - {{:https://developer.github.com/v3/#http-verbs}generic accessor functions},
        not normally used directly, but useful if you wish to invoke
        API endpoints not yet bound.
      - handler constructors to help with using the generic accessors
      - monad state injectors for setting things like default tokens or
        user agent strings
      - cached, rate limit queries
      - error message utility functions
  *)
  module API : sig
    val code_handler : expected_code:Cohttp.Code.status_code -> 'a -> 'a handler
    (** [code_handler ~expected_code parse] is a response handler that
        fires for responses with status [expected_code] and applies
        [parse]. *)

    val get :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      'a parse -> 'a Monad.t
    (** [get ?rate ?fail_handlers ?expected_code ?headers ?token
        ?params uri p] is the [p]-parsed response to a GitHub API HTTP
        GET request to [uri] with extra query parameters [params] and
        extra headers [headers]. If [token] is supplied, it will be
        used instead of any token bound into the monad. [p] will only
        fire if the response status is [expected_code] which defaults
        to [200 OK]. If the response status is not [expected_code],
        [fail_handlers], if any, will be checked in the order
        supplied. The [rate] parameter determines which rate limit
        accounting regime will be used for caching rate limit values
        in response headers. *)

    val get_stream :
      ?rate:rate ->
      ?fail_handlers:'a Stream.parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a Stream.parse -> 'a Stream.t
    (** [get_stream uri stream_p] is the {!Stream.t} encapsulating
        lazy [stream_p]-parsed responses to GitHub API HTTP GET
        requests to [uri] and
        {{:https://developer.github.com/v3/#pagination}its
        successors}. For an explanation of the other
        parameters, see {!get}. *)

    val post :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse -> 'a Monad.t
    (** [post uri p] is the [p]-parsed response to a GitHub API HTTP
        POST request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val delete :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t -> 
      ?token:Token.t -> 
      ?params:(string * string) list ->
      uri:Uri.t -> 
      'a parse -> 'a Monad.t
    (** [delete uri p] is the [p]-parsed response to a GitHub API HTTP
        DELETE request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val patch :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse -> 'a Monad.t
    (** [patch uri p] is the [p]-parsed response to a GitHub API HTTP
        PATCH request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val put :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      expected_code:Cohttp.Code.status_code ->
      ?headers:Cohttp.Header.t ->
      ?body:string ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse -> 'a Monad.t
    (** [put uri p] is the [p]-parsed response to a GitHub API HTTP
        PUT request to [uri]. For an explanation of the other
        parameters, see {!get}. *)

    val set_user_agent : string -> unit Monad.t
    (** [set_user_agent ua] contains monadic state that will cause
        bound requests to use the [User-Agent] header value of [ua]. *)

    val set_token : Token.t -> unit Monad.t
    (** [set_token token] contains monadic state that will cause bound
        requests to use [token] for authentication by default. This
        function enables the creation of large, generic monadic
        compositions that do not have to be parameterized by
        authentication token. *)

    val get_rate : ?rate:rate -> ?token:Token.t -> unit -> Github_t.rate Monad.t
    (** [get_rate ?rate ()] is the, possibly cached, rate limit
        information for the rate limit regime [?rate] (default {!const:Core}). *)

    val get_rate_limit : ?token:Token.t -> unit -> int Monad.t
    (** [get_rate_limit ()] is the, possibly cached, {!const:Core} total request
        quota for the current token. *)

    val get_rate_remaining : ?token:Token.t -> unit -> int Monad.t
    (** [get_rate_remaining ()] is the, possibly cached, {!const:Core} remaining
        request quota for the current token. *)

    val get_rate_reset : ?token:Token.t -> unit -> float Monad.t
    (** [get_rate_reset ()] is the, possibly cached, {!const:Core} UNIX
        epoch expiry time (s) when the remaining request quota will be
        reset to the total request quota for the current token. *)

    val string_of_message : Github_t.message -> string
    (** [string_of_message message] is the English language error
        message that GitHub generated in [message]. *)
  end

  (** The [URI] module contains URI generation functions which may be
      useful for linking on the Web or passing to other GitHub API
      clients. *)
  module URI : sig
    val authorizations : Uri.t
    (** The API endpoint for creating and retrieving authorizations. *)

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
    (** [repo_pulls ~user ~repo] is the API endpoint for all pull
        requests on repo [user]/[repo]. *)

    val repo_milestones : user:string -> repo:string -> Uri.t
    (** [repo_milestones ~user ~repo] is the API endpoint for all
        milestones on repo [user]/[repo]. *)

    val milestone : user:string -> repo:string -> num:int -> Uri.t
    (** [milestone ~user ~repo] is the API endpoint for milestone
        [num] on repo [user]/[repo]. *)
  end

  (** The [Filter] module contains types used by search and
      enumeration interfaces which describe ways to perform result
      filtering directly in the GitHub API. *)
  module Filter : sig
    type state = [ `All | `Open | `Closed ]
    (** [state] is the activation state of a pull request, milestone,
        or issue. See {!Pull.for_repo}, {!Milestone.for_repo}, and
        {!Issue.for_repo}. *)

    type milestone_sort = [ `Due_date | `Completeness ]
    (** [milestone_sort] is the field by which to sort a collection of
        milestones. See {!Milestone.for_repo}. *)

    type issue_sort = [ `Created | `Updated | `Comments ]
    (** [issue_sort] is the field by which to sort a collection of
        issues. See {!Issue.for_repo}. *)

    type repo_sort = [ `Stars | `Forks | `Updated ]
    (** [repo_sort] is the field by which to sort a collection of
        repositories. See {!Search.repos}. *)

    type forks_sort = [ `Newest | `Oldest | `Stargazers ]
    (** [forks_sort] is the bias used when sorting a collection of
        forks. See {!Repo.forks}. *)

    type direction = [ `Asc | `Desc ]
    (** [direction] is the sortation precedence. *)

    type milestone = [ `Any | `None | `Num of int ]
    (** [milestone] is the filter predicate for issues. See
        {!Issue.for_repo}. *)

    type user = [ `Any | `None | `Login of string ]
    (** [user] is the filter predicate for issues. See
        {!Issue.for_repo}. *)

    type 'a range = [
      | `Range of 'a option * 'a option
      | `Lt of 'a
      | `Lte of 'a
      | `Eq of 'a
      | `Gte of 'a
      | `Gt of 'a
    ]
    (** ['a range] is the type of range expressions in search
        queries. [`Range] is inclusive. See {!qualifier}. *)

    type repo_field = [
      | `Name
      | `Description
      | `Readme
    ]
    (** [repo_field] is a repository search field selector. See [`In]
        in {!qualifier}. *)

    type date = string
    (** [date] is the YYYY-MM-DD representation of a day. *)

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
    (** [qualifier] is the type of repository search query predicates. *)

  end

  (** {4 API Modules} *)

  (** The [Rate_limit] module contains explicit rate limit API request
      functions which do not {i read} the rate limit cache but do {i
      write} to it. *)
  module Rate_limit : sig
    val all : ?token:Token.t -> unit -> Github_t.rate_resources Monad.t
    (** [all ()] is the current token's rate limit information for all
        rate limiting regimes. *)

    val for_core : ?token:Token.t -> unit -> Github_t.rate Monad.t
    (** [for_core ()] is the current token's rate limit information
        for the {!const:Core} rate limit regime. *)

    val for_search : ?token:Token.t -> unit -> Github_t.rate Monad.t
    (** [for_search ()] is the current token's rate limit information
        for the {!const:Search} rate limit regime. *)

  end

  (** The [User] module provides basic user information query functions. *)
  module User : sig
    val current_info : ?token:Token.t -> unit -> Github_t.user_info Monad.t
    (** [current_info ()] is the user information linked to the
        current token. *)

    val info :
      ?token:Token.t -> user:string -> unit -> Github_t.user_info Monad.t
    (** [info ~user ()] is the user information for user [user]. *)

    val repositories :
      ?token:Token.t ->
      user:string -> unit -> Github_t.repository Stream.t
    (** [repositories ~user ()] is a stream of user [user]'s repositories. *)
  end

  (** The [Organization] module exposes the functionality of the
      GitHub {{:https://developer.github.com/v3/orgs/}organization
      API}. *)
  module Organization : sig
    val teams :
      ?token:Token.t ->
      org:string ->
      unit -> Github_t.team Stream.t
    (** [teams ~org ()] is a stream of teams belonging to the
        organization [org]. *)
  end

  (** The [Team] module contains functionality relating to GitHub's
      {{:https://developer.github.com/v3/orgs/teams/}team API}. *)
  module Team : sig
    val info :
      ?token:Token.t ->
      num:int ->
      unit -> Github_t.team_info Monad.t
    (** [info ~num ()] is a description of team [num]. *)

    val repositories :
      ?token:Token.t ->
      num:int ->
      unit -> Github_t.repository Stream.t
    (** [repositories ~num ()] is a stream of repositories belonging
        to team [num]. *)
  end

  (** The [Event] module exposes GitHub's
      {{:https://developer.github.com/v3/activity/events/}event API}
      functionality. *)
  module Event : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all events for
        [user]/[repo]. *)

    val for_repo_issues :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t
    (** [for_repo_issues ~user ~repo ()] is a stream of all issue
        events for [user]/[repo]. *)

    val public_events : unit -> Github_t.event Stream.t
    (** [public_events ()] is a stream of all public events on GitHub. *)

    val for_network :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t
    (** [for_network ~user ~repo ()] is a stream of all events for the
        fork network containing [user]/[repo]. *)

    val for_org :
      ?token:Token.t ->
      org:string -> unit -> Github_t.event Stream.t
    (** [for_org ~org ()] is a stream of all events for the
        organization [org]. *)

    val for_org_member :
      ?token:Token.t ->
      user:string ->
      org:string -> unit -> Github_t.event Stream.t
    (** [for_org_member ~user ~org ()] is a stream of [org] events
        which [user] receives. *)

    val received_by_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
    (** [received_by_user ~user ()] is a stream of all of the events
        [user] receives. If the current token is for [user], public
        and private events will be returned. If not, only public
        events will be returned. *)

    val public_received_by_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
    (** [public_received_by_user ~user ()] is a stream of the public
        events [user] receives. *)

    val for_user :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
    (** [for_user ~user ()] is a stream of the events generated by
        [user]. If the current token is for [user], public and private
        events will be returned. If not, only public events will be
        returned. *)

    val for_user_public :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
    (** [for_user_public ~user ()] is a stream of the public events
        generated by [user]. *)
  end

  (** The [Repo] module offers the functionality of GitHub's
      {{:https://developer.github.com/v3/repos/}repository API}. *)
  module Repo : sig
    val info :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repository Monad.t
    (** [info ~user ~repo ()] is a description of repository [user]/[repo]. *)

    val fork :
      ?token:Token.t ->
      ?organization:string ->
      user:string -> repo:string ->
      unit -> Github_t.repository Monad.t
    (** [fork ?organization ~user ~repo ()] is a newly forked
        repository from [user]/[repo] to the current token's user or
        [organization] if it's provided. *)

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
    (** [branches ~user ~repo ()] is a stream of all branches in repo
        [user]/[repo]. *)

    val refs :
      ?token:Token.t ->
      ?ty:string -> user:string -> repo:string ->
      unit -> Github_t.git_ref Stream.t
    (** [refs ?ty ~user ~repo ()] is a stream of all
        {{:https://developer.github.com/v3/git/refs/}git references}
        with prefix [?ty] for repo [user]/[repo]. *)

    val commit :
      ?token:Token.t ->
      user:string -> repo:string -> sha:string ->
      unit -> Github_t.commit Monad.t
    (** [commit ~user ~repo ~sha ()] is commit [sha] in [user]/[repo]. *)
  end

  (** The [Hook] module provides access to GitHub's
      {{:https://developer.github.com/v3/repos/hooks/}webhooks API}
      which lets you manage a repository's post-receive hooks. *)
  module Hook : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.hook Stream.t
    (** [for_repo ~user ~repo ()] is a stream of hooks for repo [user]/[repo]. *)

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.hook Monad.t
    (** [get ~user ~repo ~num ()] is hook [num] for repo [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      hook:Github_t.new_hook -> unit -> Github_t.hook Monad.t
    (** [create ~user ~repo ~hook ()] is a newly created post-receive
        hook for repo [user]/[repo] as described by [hook]. *)

    val update :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      hook:Github_t.update_hook -> unit -> Github_t.hook Monad.t
    (** [update ~user ~repo ~num ~hook ()] is the updated hook [num]
        in [user]/[repo] as described by [hook]. *)

    val delete :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> unit Monad.t
    (** [delete ~user ~repo ~num ()] activates after hook [num] in
        repo [user]/[repo] has been deleted. *)

    val test :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> unit Monad.t
    (** [test ~user ~repo ~num ()] activates after a [push] event
        for the lastest push to [user]/[repo] has been synthesized
        and sent to hook [num]. *)
  end

  (** The [Status] module provides the functionality of GitHub's
      {{:https://developer.github.com/v3/repos/statuses/}status API}. *)
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
    (** [create ~user ~repo ~sha ~status ()] is a newly created status
        on SHA [sha] in repo [user]/[repo] as described by [status]. *)
  end

  (** The [Pull] module contains functionality relating to GitHub's
      {{:https://developer.github.com/v3/pulls/}pull request API}. *)
  module Pull : sig
    val for_repo :
      ?state:Filter.state ->
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.pull Stream.t
    (** [for_repo ?state ~user ~repo ()] is a stream of pull requests
        against repo [user]/[repo] which are currently in state
        [?state] (default [`Open]). *)

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.pull Monad.t
    (** [get ~user ~repo ~num ()] is the pull request [user]/[repo]#[num]. *)

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      pull:Github_t.new_pull -> unit -> Github_t.pull Monad.t
    (** [create ~user ~repo ~pull ()] is the newly created pull
        request against repo [user]/[repo] as described by [pull]. *)

    val create_from_issue :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      pull_issue:Github_t.new_pull_issue ->
      unit -> Github_t.pull Monad.t
    (** [create_from_issue ~user ~repo ~pull_issue ()] is the newly
        created pull request from an issue against repo [user]/[repo]
        as described by [pull_issue]. *)

    val update :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      update_pull:Github_t.update_pull ->
      num:int -> unit -> Github_t.pull Monad.t
    (** [update ~user ~repo ~update_pull ~num ()] is the updated pull
        request [user]/[repo]#[num] as described by [update_pull]. *)

    val list_commits :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.commit Stream.t
    (** [list_commits ~user ~repo ~num ()] is the stream of commits
        included in pull request [user]/[repo]#[num]. *)

    val list_files :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.file Stream.t
    (** [list_files ~user ~repo ~num ()] is the stream of files
        included in pull request [user]/[repo]#[num]. *)

    val is_merged :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> bool Monad.t
    (** [is_merged ~user ~repo ~num ()] is [true] if pull request
        [user]/[repo]#[num] has been merged. *)

    val merge :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      ?merge_commit_message:string -> unit -> Github_t.merge Monad.t
    (** [merge ~user ~repo ~num ?merge_commit_message ()] is the merge
        of pull request [user]/[repo]#[num] with optional commit
        message [?merge_commit_message]. *)
  end

  (** The [Issue] module gives users access to GitHub's
      {{:https://developer.github.com/v3/issues/}issue API}. *)
  module Issue: sig
    val for_repo :
      ?token:Token.t -> ?creator:string -> ?mentioned:string ->
      ?assignee:Filter.user ->
      ?labels:string list -> ?milestone:Filter.milestone ->
      ?state:Filter.state ->
      ?sort:Filter.issue_sort -> ?direction:Filter.direction ->
      user:string -> repo:string -> unit -> Github_t.issue Stream.t
    (** [for_repo ?creator ?mentioned ?assignee ?labels ?milestone
        ?state ?sort ?direction ~user ~repo ()] is a stream of issues
        in repo [user]/[repo] which were created by user [?creator],
        mention user [?mentioned], are assigned to user [?assignee],
        have labels [?labels], are included in milestone [?milestone],
        and are in state [?state]. The stream is sorted by [?sort]
        (default [`Created]) and ordered by [?direction] (default
        [`Desc]). *)

    val create :
      ?token:Token.t -> user:string -> repo:string ->
      issue:Github_t.new_issue -> unit -> Github_t.issue Monad.t
    (** [create ~user ~repo ~issue ()] is a newly created issue
        described by [issue] in repo [user]/[repo]. *)

    val update :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> issue:Github_t.new_issue ->
      unit -> Github_t.issue Monad.t
    (** [update ~user ~repo ~num ~issue ()] is the updated issue [num]
        in [user]/[repo] as described by [issue]. *)

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
    (** [is_issue issue] is true if [issue] is an actual issue and not
        a pull request. *)

    val is_pull : Github_t.issue -> bool
    (** [is_pull issue] is true if [issue] is a pull request. *)
  end

  (** The [Milestone] module exposes GitHub's
      {{:https://developer.github.com/v3/issues/milestones/}milestone
      API}. *)
  module Milestone : sig
    val for_repo:
      ?state:Filter.state ->
      ?sort:Filter.milestone_sort ->
      ?direction:Filter.direction ->
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.milestone Stream.t
    (** [for_repo ?state ?sort ?direction ~user ~repo ()] is a stream
        of all milestones in repo [user]/[repo] which match [?state]
        (default [`Open]). The stream is sorted by [?sort] (default
        [`Due_date]) and ordered by [?direction] (default [`Desc]). *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> Github_t.milestone Monad.t
    (** [get ~user ~repo ~num ()] is milestone number [num] in repo
        [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      milestone:Github_t.new_milestone -> unit -> Github_t.milestone Monad.t
    (** [create ~user ~repo ~milestone ()] is the newly created
        milestone described by [milestone] in repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> unit Monad.t
    (** [delete ~user ~repo ~num ()] activates after milestone
        [num] in repo [user]/[repo] has been deleted. *)

    val update :
      ?token:Token.t ->
      user:string -> repo:string ->
      milestone:Github_t.update_milestone -> num:int ->
      unit -> Github_t.milestone Monad.t
    (** [update ~user ~repo ~milestone ~num ()] is the updated
        milestone [num] in repo [user]/[repo] as described by
        [milestone]. *)
  end

  (** The [Release] module provides access to GitHub's
      {{:https://developer.github.com/v3/repos/releases/}release API}
      features. *)
  module Release : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.release Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all releases in repo
        [user]/[repo]. *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> Github_t.release Monad.t
    (** [get ~user ~repo ~num ()] is release number [num] in repo
        [user]/[repo]. *)

    val get_by_tag_name:
      ?token:Token.t ->
      user:string -> repo:string -> tag:string -> unit -> Github_t.release Monad.t
    (** [get_by_tag_name ~user ~repo ~tag ()] is the release in repo
        [user]/[repo] which is using git tag [tag]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.new_release -> unit -> Github_t.release Monad.t
    (** [create ~user ~repo ~release ()] is the newly created release
        described by [release] in repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> unit Monad.t
    (** [delete ~user ~repo ~num ()] activates after release [num]
        in repo [user]/[repo] has been deleted. *)

    val update :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.update_release -> num:int ->
      unit -> Github_t.release Monad.t
    (** [update ~user ~repo ~release ~num ()] is the updated release
        [num] in [user]/[repo] as described by [release]. *)

    val upload_asset :
      ?token:Token.t ->
      user:string -> repo:string ->
      num:int -> filename:string -> content_type:string ->
      body:string ->
      unit -> unit Monad.t
    (** [upload_asset ~user ~repo ~num ~filename ~content_type ~body ()]
        activates after [body] is uploaded to repo [user]/[repo] as
        an asset for release [num] with file name [filename] and content
        type [content_type]. *)
  end

  (** The [Deploy_key] module provides the means to manage
      per-repository
      {{:https://developer.github.com/guides/managing-deploy-keys/#deploy-keys}deploy
      keys}.
      @see <https://developer.github.com/v3/repos/keys/> deploy key API docs
  *)
  module Deploy_key : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.deploy_key Stream.t
    (** [for_repo ~user ~repo ()] is a stream of deploy keys
        associated with repo [user]/[repo]. *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> Github_t.deploy_key Monad.t
    (** [get ~user ~repo ~num ()] is deploy key [num] for repo [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      new_key:Github_t.new_deploy_key -> unit -> Github_t.deploy_key Monad.t
    (** [create ~user ~repo ~new_key ()] is the newly created deploy
        key [new_key] for repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int -> unit -> unit Monad.t
    (** [delete ~user ~repo ~num ()] activates after deploy key
        [num] in repo [user]/[repo] has been deleted. *)
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
    (** [star ~num ()] activates after gist [num] is marked as
        starred by the current token's user. *)

    val unstar :
      ?token:Token.t ->
      num:string -> unit -> unit Monad.t
    (** [unstar ~num ()] activates after gist [num] is marked as
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
    (** [delete ~num ()] activates after gist [num] has been deleted. *)
  end

  (** The [Search] module exposes GitHub's
      {{:https://developer.github.com/v3/search/}search interfaces}. *)
  module Search : sig
    val repos :
      ?token:Token.t ->
      ?sort:Filter.repo_sort ->
      ?direction:Filter.direction ->
      qualifiers:Filter.qualifier list ->
      keywords:string list ->
      unit -> Github_t.repository_search Stream.t
    (** [repos ?sort ?direction ~qualifiers ~keywords ()] is a
        stream of repository search results for [keywords] and
        matching [qualifiers] predicates. Results are sorted by
        [?sort] (default best match) and ordered by [?direction]
        (default [`Desc]). *)
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

(** A module of this type is required in order to construct a
    {!Github} module using {!Github_core.Make}. *)
module type Time = sig
  val now : unit -> float
  (** [now ()] is the current UNIX epoch time in seconds. *)

  val sleep : float -> unit Lwt.t
  (** [sleep sec] activates after [sec] seconds have elapsed. *)
end
