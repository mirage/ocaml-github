(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2016 David Sheets <sheets@alum.mit.edu>
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
   {3 GitHub APIv3 client library}

   This library offers thin but natural bindings to
   {{:https://docs.github.com/rest}GitHub's developer API}.
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

  (** Functions corresponding to direct API requests return
      {!Response.t} values inside of {!Monad.t} values so that more
      information about the request can be made
      available. {!Monad.(>>~)} is a convenience operator that lets
      you bind directly to the carried value. *)
  module Response : sig
    type redirect =
      | Temporary of Uri.t (** The redirection is temporary. *)
      | Permanent of Uri.t (** The redirection is permanent. *)
    (** [redirect] indicates whether the originally requested
        endpoint should continue to be used in the future. *)

    type 'a t = private < value : 'a; redirects : redirect list; .. >
    (** ['a t] is an API response containing a payload of type
        ['a]. {b Do not} refer to this type explicitly as its identity and
        representation are subject to change (e.g. a family of object
        types may replace it before 3.0). *)

    val value : < value : 'a; .. > -> 'a
    (** [value r] is the payload in response [r]. *)

    val redirects : < redirects : redirect list; .. > -> redirect list
    (** [redirects r] is the sequence of redirects prior to response [r]. *)

    val final_resource : redirect list -> redirect option
    (** [final_resource rs] is the single redirect, if any redirects
        occurred, that describes the overall redirect chain [rs]. If
        any redirect [rs] is temporary, [final_resource rs] will be a
        temporary redirect to the final URI. If all redirects [rs] are
        permanent, [final_resource rs] will be a permanent redirect to
        the final URI. *)
  end

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
    (** [m >>= f] is [{!bind} f m]. *)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** [m >|= f] is [{!map} f m]. *)

    val (>>~) : 'a Response.t t -> ('a -> 'b t) -> 'b t
    (** [m >>~ f] is [m >|= {!Response.value} >>= f]. *)

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    (** [catch try with] is the result of trying [try]. If [try]
        succeeds, its result is returned. If [try] raises an
        exception, [with] is applied to the exception and the result
        of [with] is returned. *)

    val fail : exn -> 'a t
    (** [fail exn] raises exception [exn] inside of the monad. *)

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

  (** Each request to GitHub is made to a specific [Endpoint] in
      GitHub's REST-like API. *)
  module Endpoint : sig
    (** Some endpoints expose resources which change over time and
        responses from those endpoints may contain version metadata
        which can be used to make low-cost conditional requests
        (e.g. cache validation). *)
    module Version : sig
      type t =
        | Etag of string (** An entity tag identifier *)
        | Last_modified of string
          (** A timestamp conforming to the HTTP-date production *)
          (** [t] is a version of a resource representation. *)
    end
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

    val fold : ('a -> 'b -> 'a Monad.t) -> 'a -> 'b t -> 'a Monad.t
    (** [fold f a s] is the left fold of [f] over the elements of [s]
        with a base value of [a]. {b Warning:} this function may
        result in {i many} successive API transactions. *)

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

    val since : 'a t -> Endpoint.Version.t -> 'a t
    (** [since stream version] is [stream] with [version] but without
        any other change, i.e. the stream is not reset to its
        beginning. Used in conjunction with [poll], [since] enables
        low-cost conditional re-synchronization of local state with
        GitHub state. *)

    val version : 'a t -> Endpoint.Version.t option
    (** [version stream] is the version of [stream] if one is
        known. After any stream element is forced, the stream version
        will be available unless GitHub violates its API specification. *)
  end

  type rate = Core | Search (**)
  (** [rate] is a type used to indicate which
      {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#rate-limiting}rate-limiting
      regime} is to be used for query quota accounting. [rate] is used by the
      function in {!API}. *)

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

  type 'a handler = (Cohttp.Response.t * string -> bool) * 'a
  (** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). *)

  val log_active : bool ref
  (** [log_active] regulates debug messages. It is [true] by default
      when the environment variable [GITHUB_DEBUG] is set to 1. *)

  (** The [Scope] module abstracts GitHub's
      {{:https://docs.github.com/developers/apps/scopes-for-oauth-apps#available-scopes}authorization
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
      {{:https://docs.github.com/developers/apps/authorizing-oauth-apps}OAuth
      application tokens} and
      {{:https://docs.github.com/github/authenticating-to-github/creating-a-personal-access-token}
      "personal tokens"}.

      Note: the OAuth Authorizations API has been deprecated by GitHub.

      @see <https://docs.github.com/rest/reference/oauth-authorizations> OAuth
      Authorizations API
      @see <https://developer.github.com/changes/2019-11-05-deprecated-passwords-and-authorizations-api/#deprecating-and-adding-endpoints-for-the-oauth-authorizations-and-oauth-applications>
      for the OAuth Authorizations deprecation.
  *)
  module Token : sig
    type t
    (** [t] is the abstract type of a token. *)

    val of_code :
      client_id:string -> client_secret:string -> code:string ->
      unit -> t option Lwt.t
    (** [of_code ~client_id ~client_secret ~code ()] is the {!t}
        granted by a [code] from an
        {{:https://docs.github.com/developers/apps/authorizing-oauth-apps#web-application-flow}
        OAuth web flow redirect}. *)

    val create : ?scopes:Github_t.scope list -> ?note:string ->
      ?note_url:string -> ?client_id:string -> ?client_secret:string ->
      ?fingerprint:string -> ?otp:string ->
      user:string -> pass:string -> unit ->
      Github_t.auth authorization Response.t Monad.t
    (** [create ?otp ~user ~pass ()] is a new authorization with the
        provided fields. When a user has enabled two-factor
        authentication, the return value will be a {!constructor:Two_factor}
        constructor with the one-time password delivery
        mode. Including a valid [?otp] will yield a {!constructor:Result} return
        value. *)

    val get_all : ?otp:string -> user:string -> pass:string -> unit ->
      Github_t.auths authorization Response.t Monad.t
    (** [get_all ~user ~pass ()] are all of the authorizations that
        this user has made. See {!create} for an explanation of how
        two-factor authentication is handled. *)

    val get : ?otp:string -> user:string -> pass:string -> id:int64 ->
      unit -> Github_t.auth option authorization Response.t Monad.t
    (** [get ~user ~pass ~id ()] is the authorization with identifier
        [id]. See {!create} for an explanation of how two-factor
        authentication is handled. *)

    val delete : ?otp:string -> user:string -> pass:string -> id:int64 -> unit ->
      unit authorization Response.t Monad.t
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

      - {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#http-verbs}generic accessor functions},
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
      ?media_type:string ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse -> 'a Response.t Monad.t
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
      ?media_type:string ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a Stream.parse -> 'a Stream.t
    (** [get_stream uri stream_p] is the {!Stream.t} encapsulating
        lazy [stream_p]-parsed responses to GitHub API HTTP GET
        requests to [uri] and
        {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#pagination}its
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
      'a parse -> 'a Response.t Monad.t
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
      'a parse -> 'a Response.t Monad.t
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
      'a parse -> 'a Response.t Monad.t
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
      'a parse -> 'a Response.t Monad.t
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
    (** [get_rate ?rate ()] is the, possibly cached, rate limit information for
        the rate limit regime [?rate] (default {!constructor:Core}). *)

    val get_rate_limit : ?token:Token.t -> unit -> int Monad.t
    (** [get_rate_limit ()] is the, possibly cached, {!constructor:Core} total
        request quota for the current token. *)

    val get_rate_remaining : ?token:Token.t -> unit -> int Monad.t
    (** [get_rate_remaining ()] is the, possibly cached, {!constructor:Core}
        remaining request quota for the current token. *)

    val get_rate_reset : ?token:Token.t -> unit -> float Monad.t
    (** [get_rate_reset ()] is the, possibly cached, {!constructor:Core} UNIX
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
        {{:https://docs.github.com/developers/apps/authorizing-oauth-apps#redirect-urls}
        redirect users} to in an OAuth2 flow to create an authorization
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

    type issue_comment_sort = [ `Created | `Updated ]
    (** [issue_comment_sort] is the field by which to sort a collection of
        issue comments. See {!Issue.comments_for_repo}. *)

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

    type issue_qualifier = [
      | `Author of string
      | `Assignee of string
      | `Mentions of string
      | `Commenter of string
      | `Involves of string
      | `Team of string
      | `Label of string
      | `Without_label of string
      | `Language of string
      | `Created of date range
      | `Updated of date range
      | `Merged of date range
      | `Closed of date range
      | `User of string
      | `Repo of string
      | `Project of string
    ]
    (** [issue_qualifier] is the type of issue search query predicates. *)

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
        for the {!constructor:Core} rate limit regime. *)

    val for_search : ?token:Token.t -> unit -> Github_t.rate Monad.t
    (** [for_search ()] is the current token's rate limit information
        for the {!constructor:Search} rate limit regime. *)

  end

  (** The [User] module provides basic user information query functions. *)
  module User : sig
    val current_info : ?token:Token.t ->
      unit -> Github_t.user_info Response.t Monad.t
    (** [current_info ()] is the user information linked to the
        current token. *)

    val info :
      ?token:Token.t -> user:string ->
      unit -> Github_t.user_info Response.t Monad.t
    (** [info ~user ()] is the user information for user [user]. *)

    val repositories :
      ?token:Token.t ->
      user:string -> unit -> Github_t.repository Stream.t
    (** [repositories ~user ()] is a stream of user [user]'s repositories. *)
  end

  (** The [Organization] module exposes the functionality of the
      GitHub {{:https://docs.github.com/rest/reference/orgs}organization
      API}. *)
  module Organization : sig
    val teams :
      ?token:Token.t ->
      org:string ->
      unit -> Github_t.team Stream.t
    (** [teams ~org ()] is a stream of teams belonging to the
        organization [org]. *)

    val user_orgs :
      ?token:Token.t ->
      user:string ->
      unit -> Github_t.org Stream.t
    (** [user_orgs ~user ()] is a stream of the organizations
         to which the user [user] belongs. *)

    val current_user_orgs:
      ?token:Token.t ->
      unit -> Github_t.org Stream.t
    (** [current_user ()] is a stream of the organizations to which
        the user linked to current token belongs, and for which the user
        granted access to the organizations to the current token. *)

    val repositories:
      ?token:Token.t ->
      org:string  ->
      unit ->
      Github_t.repository Stream.t
    (** [repositories ~org ()] is a stream of repositories belonging to the
        organization [org]. *)

    (** The [Hook] module provides access to GitHub's
        {{:https://docs.github.com/rest/reference/orgs#webhooks}organization
        webhooks API} which lets you manage an organization's
        remote notification hooks. *)
    module Hook : sig
      val for_org :
        ?token:Token.t ->
        org:string -> unit -> Github_t.hook Stream.t
      (** [for_org ~org ()] is a stream of hooks for the organization [org]. *)

      val get :
        ?token:Token.t ->
        org:string -> id:int64 -> unit -> Github_t.hook Response.t Monad.t
      (** [get ~org ~id ()] is hook [id] for organization [org]. *)

      val create :
        ?token:Token.t ->
        org:string ->
        hook:Github_t.new_hook -> unit -> Github_t.hook Response.t Monad.t
      (** [create ~org ~hook ()] is a newly created post-receive
          hook for organization [org] as described by [hook]. *)

      val update :
        ?token:Token.t ->
        org:string ->
        id:int64 ->
        hook:Github_t.update_hook -> unit -> Github_t.hook Response.t Monad.t
      (** [update ~org ~id ~hook ()] is the updated hook [id] for
          organization [org] as described by [hook]. *)

      val delete :
        ?token:Token.t ->
        org:string -> id:int64 -> unit -> unit Response.t Monad.t
      (** [delete ~org ~id ()] activates after hook [id] in
          organization [org] has been deleted. *)

      val test :
        ?token:Token.t ->
        org:string -> id:int64 -> unit -> unit Response.t Monad.t
      (** [test ~org ~id ()] activates after a [push] event for
          the lastest push for organization [org] has been synthesized and
          sent to hook [id]. *)

      val parse_event :
        constr:string ->
        payload:string -> unit -> Github_t.event_hook_constr
        (** [parse_event ~constr ~payload ()] is the event with
            constructor [constr] that is represented by [payload]. *)

      val parse_event_metadata :
        payload:string -> unit -> Github_t.event_hook_metadata
        (** [parse_event_metadata ~payload ()] is the event metadata for
            the serialized event [payload]. *)
    end

  end

  (** The [Team] module contains functionality relating to GitHub's
      {{:https://docs.github.com/rest/reference/teams}team API}. *)
  module Team : sig
    val info :
      ?token:Token.t ->
      id:int64 ->
      unit -> Github_t.team_info Response.t Monad.t
    (** [info ~id ()] is a description of team [id]. *)

    val repositories :
      ?token:Token.t ->
      id:int64 ->
      unit -> Github_t.repository Stream.t
    (** [repositories ~id ()] is a stream of repositories belonging
        to team [id]. *)
  end

  (** The [Event] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/activity#events}event API}
      functionality. *)
  module Event : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.event Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all events for
        [user]/[repo]. *)

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

    val received_by_user_public :
      ?token:Token.t ->
      user:string -> unit -> Github_t.event Stream.t
    (** [received_by_user_public ~user ()] is a stream of the public
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
      {{:https://docs.github.com/rest/reference/repos}repository API}. *)
  module Repo : sig
    val create :
      ?token:Token.t ->
      ?organization:string ->
      repo:Github_t.new_repo ->
      unit -> Github_t.repository Response.t Monad.t
    (** [create ?organization new_repo ()] is a new repository owned
        by the user or organizations if it's provided. *)

    val info :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.repository Response.t Monad.t
    (** [info ~user ~repo ()] is a description of repository [user]/[repo]. *)

    val fork :
      ?token:Token.t ->
      ?organization:string ->
      user:string -> repo:string ->
      unit -> Github_t.repository Response.t Monad.t
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
      unit -> Github_t.tag Response.t Monad.t
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
        {{:https://docs.github.com/rest/reference/git#references}git references}
        with prefix [?ty] for repo [user]/[repo]. *)

    val get_ref :
      ?token:Token.t ->
      user:string -> repo:string ->
      name:string ->
      unit -> Github_t.git_ref Response.t Monad.t
    (** [get_ref ~user ~repo ~name] is the
        {{:https://docs.github.com/rest/reference/git#references}git reference}
        with name [name] for repo [user]/[repo]. *)

    val get_commit :
      ?token:Token.t ->
      user:string -> repo:string -> sha:string ->
      unit -> Github_t.commit Response.t Monad.t
    (** [get_commit ~user ~repo ~sha ()] is commit [sha] in [user]/[repo]. *)

    val contributors :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.contributor Stream.t
    (** [contributors ~user ~repo ()] is a stream of contributors to
        repo [user]/[repo]. *)

    val delete :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> unit Response.t Monad.t
    (** [delete ~user ~repo ()] activates after repo [user]/[repo] has
        been deleted. *)

    (** The [Hook] module provides access to GitHub's
        {{:https://docs.github.com/rest/reference/repos#webhooks}webhooks API}
        which lets you manage a repository's post-receive hooks. *)
    module Hook : sig
      val for_repo :
        ?token:Token.t ->
        user:string ->
        repo:string -> unit -> Github_t.hook Stream.t
      (** [for_repo ~user ~repo ()] is a stream of hooks for repo
          [user]/[repo]. *)

      val get :
        ?token:Token.t ->
        user:string ->
        repo:string -> id:int64 -> unit -> Github_t.hook Response.t Monad.t
      (** [get ~user ~repo ~id ()] is hook [id] for repo [user]/[repo]. *)

      val create :
        ?token:Token.t ->
        user:string ->
        repo:string ->
        hook:Github_t.new_hook -> unit -> Github_t.hook Response.t Monad.t
      (** [create ~user ~repo ~hook ()] is a newly created post-receive
          hook for repo [user]/[repo] as described by [hook]. *)

      val update :
        ?token:Token.t ->
        user:string ->
        repo:string ->
        id:int64 ->
        hook:Github_t.update_hook -> unit -> Github_t.hook Response.t Monad.t
      (** [update ~user ~repo ~id ~hook ()] is the updated hook [id] in
          repo [user]/[repo] as described by [hook]. *)

      val delete :
        ?token:Token.t ->
        user:string ->
        repo:string -> id:int64 -> unit -> unit Response.t Monad.t
      (** [delete ~user ~repo ~id ()] activates after hook [id] in repo
          [user]/[repo] has been deleted. *)

      val test :
        ?token:Token.t ->
        user:string ->
        repo:string -> id:int64 -> unit -> unit Response.t Monad.t
      (** [test ~user ~repo ~id ()] activates after a [push] event for
          the lastest push to [user]/[repo] has been synthesized and
          sent to hook [id]. *)

      val parse_event :
        constr:string ->
        payload:string -> unit -> Github_t.event_hook_constr
        (** [parse_event ~constr ~payload ()] is the event with
            constructor [constr] that is represented by [payload]. *)

      val parse_event_metadata :
        payload:string -> unit -> Github_t.event_hook_metadata
        (** [parse_event_metadata ~payload ()] is the event metadata for
            the serialized event [payload]. *)
    end

  end

  (** The [Stats] module exposes the functionality of GitHub's
      {{:https://docs.github.com/rest/reference/repos#statistics}repository
      statistics API} which provides historical data regarding the
      aggregate behavior of a repository. *)
  module Stats : sig
    val contributors :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.contributor_stats Stream.t
    (** [contributors ~user ~repo ()] is a stream of all contributor
        statistics for [user]/[repo]. The stream is empty if the
        data are not cached yet. *)

    val yearly_commit_activity :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.commit_activity Stream.t
    (** [yearly_commit_activity ~user ~repo ()] returns the last year of commit
        activity grouped by week for [user]/[repo]. The days array is a group of
        commits per day, starting on Sunday. The stream is empty if the data are
        not cached yet. *)

    val weekly_commit_activity :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.code_frequency Stream.t
    (** [weekly_commit_activity ~user ~repo ()] returns a weekly aggregate of
        the number of additions and deletions pushed to [user]/[repo]. The
        stream is empty if the data are not cached yet. *)

    val weekly_commit_count :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.participation Response.t Monad.t
    (** [weekly_commit_count ~user ~repo ()] returns the total commit counts for
        the owner and total commit counts in all. all is everyone combined,
        including the owner in the last 52 weeks. If you'd like to get the
        commit counts for non-owners, you can subtract owner from all.

        The array order is oldest week (index 0) to most recent week.*)

    val hourly_commit_count :
      ?token:Token.t ->
      user:string -> repo:string ->
      unit -> Github_t.punch_card Stream.t
    (** [hourly_commit_count ~user ~repo ()] returns the hourly commit count for
        each day.
        Each array contains the day number, hour number, and number of commits:
         - 0-6: Sunday - Saturday
         - 0-23: Hour of day
         - Number of commits
        For example, [2, 14, 25] indicates that there were 25 total commits,
        during the 2:00pm hour on Tuesdays. All times are based on the time
        zone of individual commits.*)

  end

  (** The [Status] module provides the functionality of GitHub's
      {{:https://docs.github.com/rest/reference/repos#statuses}status API}. *)
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
      unit -> Github_t.status Response.t Monad.t
    (** [create ~user ~repo ~sha ~status ()] is a newly created status
        on SHA [sha] in repo [user]/[repo] as described by [status]. *)

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      sha:string ->
      unit -> Github_t.combined_status Response.t Monad.t
    (** [get ~user ~repo ~sha ()] is the combined status of the ref
        [sha] in the repo [user]/[repo]. *)
  end

  (** The [Pull] module contains functionality relating to GitHub's
      {{:https://docs.github.com/rest/reference/pulls}pull request API}. *)
  module Pull : sig
    val for_repo :
      ?token:Token.t ->
      ?state:Filter.state ->
      user:string ->
      repo:string -> unit -> Github_t.pull Stream.t
    (** [for_repo ?state ~user ~repo ()] is a stream of pull requests
        against repo [user]/[repo] which are currently in state
        [?state] (default [`Open]). *)

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.pull Response.t Monad.t
    (** [get ~user ~repo ~num ()] is the pull request [user]/[repo]#[num]. *)

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      pull:Github_t.new_pull -> unit -> Github_t.pull Response.t Monad.t
    (** [create ~user ~repo ~pull ()] is the newly created pull
        request against repo [user]/[repo] as described by [pull]. *)

    val create_from_issue :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      pull_issue:Github_t.new_pull_issue ->
      unit -> Github_t.pull Response.t Monad.t
    (** [create_from_issue ~user ~repo ~pull_issue ()] is the newly
        created pull request from an issue against repo [user]/[repo]
        as described by [pull_issue]. *)

    val update :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      update_pull:Github_t.update_pull ->
      num:int -> unit -> Github_t.pull Response.t Monad.t
    (** [update ~user ~repo ~update_pull ~num ()] is the updated pull
        request [user]/[repo]#[num] as described by [update_pull]. *)

    val commits :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.commit Stream.t
    (** [commits ~user ~repo ~num ()] is the stream of commits
        included in pull request [user]/[repo]#[num]. *)

    val files :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.file Stream.t
    (** [files ~user ~repo ~num ()] is the stream of files
        included in pull request [user]/[repo]#[num]. *)

    val is_merged :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> bool Response.t Monad.t
    (** [is_merged ~user ~repo ~num ()] is [true] if pull request
        [user]/[repo]#[num] has been merged. *)

    val merge :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      ?merge_commit_message:string ->
      unit -> Github_t.merge Response.t Monad.t
    (** [merge ~user ~repo ~num ?merge_commit_message ()] is the merge
        of pull request [user]/[repo]#[num] with optional commit
        message [?merge_commit_message]. *)
  end

  (** The [Issue] module gives users access to GitHub's
      {{:https://docs.github.com/rest/reference/issues}issue API}. *)
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

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string -> num:int -> unit -> Github_t.issue Response.t Monad.t
    (** [get ~user ~repo ~num ()] is the issue [user]/[repo]#[num]. *)

    val create :
      ?token:Token.t -> user:string -> repo:string ->
      issue:Github_t.new_issue -> unit -> Github_t.issue Response.t Monad.t
    (** [create ~user ~repo ~issue ()] is a newly created issue
        described by [issue] in repo [user]/[repo]. *)

    val update :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> issue:Github_t.update_issue ->
      unit -> Github_t.issue Response.t Monad.t
    (** [update ~user ~repo ~num ~issue ()] is the updated issue [num]
        in [user]/[repo] as described by [issue]. *)

    val events_for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string -> unit -> Github_t.repo_issues_event Stream.t
    (** [events_for_repo ~user ~repo ()] is a stream of all issue
        events for [user]/[repo]. *)

    val events :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int -> unit -> Github_t.repo_issue_event Stream.t
    (** [events ~user ~repo ~num ()] is a stream of all issue events
        for [user]/[repo]#[num]. *)

    val timeline_events :
      ?token:Token.t -> user:string -> repo:string -> num:int -> unit ->
      Github_t.timeline_event Stream.t
    (** [timeline_events ~user ~repo ~num ()] is a stream of all timeline
        events for [user]/[repo]#[num]. *)

    val comments :
      ?token:Token.t -> ?since:string -> user:string -> repo:string ->
      num:int -> unit -> Github_t.issue_comment Stream.t
    (** [comments ?since ~user ~repo ~num ()] is a stream of issue
        comments for [user]/[repo]#[num]. If [?since], an ISO 8601
        format timestamp (YYYY-MM-DDTHH:MM:SSZ), is supplied, only
        comments updated at or after this time are returned. *)

    val comments_for_repo :
      ?token:Token.t ->
      ?sort:Filter.issue_comment_sort -> ?direction:Filter.direction ->
      ?since:string ->
      user:string -> repo:string ->
      unit -> Github_t.issue_comment Stream.t
    (** [comments_for_repo ~user ~repo ()] is a stream of issue
        comments for repo [user]/[repo] sorted by [?sort] in
        [?direction] order and having occurred since ISO 8601
        timestamp (YYYY-MM-DDTHH:MM:SSZ) [?since]. *)

    val create_comment :
      ?token:Token.t -> user:string -> repo:string ->
      num:int -> body:string ->
      unit -> Github_t.issue_comment Response.t Monad.t
    (** [create_comment ~user ~repo ~num ~body ()] is a newly created
        issue comment on [user]/[repo]#[num] with content [body]. *)

    val get_comment :
      ?token:Token.t -> user:string -> repo:string ->
      id:int64 -> unit -> Github_t.issue_comment Response.t Monad.t
    (** [get_comment ~user ~repo ~id ()] is issue comment [id] in repo
        [user]/[repo]. *)

    val update_comment :
      ?token:Token.t -> user:string -> repo:string ->
      id:int64 -> body:string ->
      unit -> Github_t.issue_comment Response.t Monad.t
    (** [update_comment ~user ~repo ~id ~body ()] is issue comment
        [id] in repo [user]/[repo] updated with content [body]. *)

    val delete_comment :
      ?token:Token.t -> user:string -> repo:string -> id:int64 ->
      unit -> unit Response.t Monad.t
    (** [delete_comment ~user ~repo ~id ()] activates after issue
        comment [id] has been deleted from repo [user]/[repo]. *)

    val labels :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      unit ->
      Github_t.label Stream.t
    (** [labels ~user ~repo ~num ()] is a stream of all labels
        applied to issue [num] in the repo [user]/[repo]. *)

    val add_labels :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      labels:string list ->
      unit ->
      Github_t.label list Response.t Monad.t
    (** [add_labels ~user ~repo ~num ~labels ()] is the list of labels
        on issue [user]/[repo]#[num] with labels [labels] added. *)

    val remove_label :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      name:string ->
      unit ->
      Github_t.label list Response.t Monad.t
    (** [remove_label ~user ~repo ~num ~name ()] is the list of labels
        on issue [user]/[repo]#[num] after [name] has been removed. *)

    val replace_labels :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      labels:string list ->
      unit ->
      Github_t.label list Response.t Monad.t
    (** [replace_labels ~user ~repo ~num ~labels ()] is the list of
         labels on issue [user]/[repo]#[num] as provided by
         [labels]. *)

    val remove_labels :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      unit ->
      unit Response.t Monad.t
    (** [remove_labels ~user ~repo ~num ()] activates after all labels
        have been removed from issue [user]/[repo]#[num]. *)

    val is_issue : Github_t.issue -> bool
    (** [is_issue issue] is true if [issue] is an actual issue and not
        a pull request. *)

    val is_pull : Github_t.issue -> bool
    (** [is_pull issue] is true if [issue] is a pull request. *)
  end

  (** The [Label] module exposes Github's
      {{:https://docs.github.com/rest/reference/issues#labels}labels
      API}. *)
  module Label : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      unit ->
      Github_t.label Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all labels in repo
        [user]/[repo]. *)

    val get :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      unit ->
      Github_t.label Response.t Monad.t
    (** [get ~user ~repo ~name ()] is the label [name] in the repo
        [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      label:Github_t.new_label ->
      unit ->
      Github_t.label Response.t Monad.t
    (** [create ~user ~repo ~label ()] is the newly created label
        [label] in the repo [user]/[repo]. *)

    val update :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      label:Github_t.new_label ->
      unit ->
      Github_t.label Response.t Monad.t
    (** [update ~user ~repo ~name ()] is the newly updated label
        [name] with properties [label] in the repo [user]/[repo]. *)

    val delete :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      unit ->
      unit Response.t Monad.t
      (** [delete ~user ~repo ~name ()] activates after the label [name]
          in the repo [user]/[repo] has been removed. *)
  end

  (** The [Collaborator] module exposes Github's
      {{:https://docs.github.com/rest/reference/repos#collaborators}
      collaborators API}. *)
  module Collaborator : sig
    val for_repo :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      unit ->
      Github_t.linked_user Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all collaborators in repo
        [user]/[repo]. *)

    val exists :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      unit ->
      bool Response.t Monad.t
    (** [exists ~user ~repo ~name ()] is true if [name] is a collaborator on
        repo [user]/[repo]. *)

    val add :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      ?permission:Github_t.team_permission ->
      unit ->
      unit Response.t Monad.t
    (** [add ~user ~repo ~name ?permission ()] adds [name] as a collaborator on
        repo [user]/[repo] with permission [permission] (default [`Push]). *)

    val remove :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      name:string ->
      unit ->
      unit Response.t Monad.t
    (** [remove ~user ~repo ~name ()] activates after [name] has been
        removed from the collaborator set on the repo [user]/[repo]. *)
  end

  (** The [Milestone] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/issues#milestones}milestone
      API}. *)
  module Milestone : sig
    val for_repo:
      ?token:Token.t ->
      ?state:Filter.state ->
      ?sort:Filter.milestone_sort ->
      ?direction:Filter.direction ->
      user:string -> repo:string -> unit -> Github_t.milestone Stream.t
    (** [for_repo ?state ?sort ?direction ~user ~repo ()] is a stream
        of all milestones in repo [user]/[repo] which match [?state]
        (default [`Open]). The stream is sorted by [?sort] (default
        [`Due_date]) and ordered by [?direction] (default [`Desc]). *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> num:int ->
      unit -> Github_t.milestone Response.t Monad.t
    (** [get ~user ~repo ~num ()] is milestone number [num] in repo
        [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      milestone:Github_t.new_milestone ->
      unit -> Github_t.milestone Response.t Monad.t
    (** [create ~user ~repo ~milestone ()] is the newly created
        milestone described by [milestone] in repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> num:int ->
      unit -> unit Response.t Monad.t
    (** [delete ~user ~repo ~num ()] activates after milestone
        [num] in repo [user]/[repo] has been deleted. *)

    val update :
      ?token:Token.t ->
      user:string -> repo:string ->
      milestone:Github_t.update_milestone -> num:int ->
      unit -> Github_t.milestone Response.t Monad.t
    (** [update ~user ~repo ~milestone ~num ()] is the updated
        milestone [num] in repo [user]/[repo] as described by
        [milestone]. *)

    val labels :
      ?token:Token.t ->
      user:string ->
      repo:string ->
      num:int ->
      unit ->
      Github_t.label Stream.t
    (** [labels ~user ~repo ~num ()] is a stream of all labels for
        milestone [num] in repo [user]/[repo]. *)
  end

  (** The [Release] module provides access to GitHub's
      {{:https://docs.github.com/rest/reference/repos#releases}release API}
      features. *)
  module Release : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.release Stream.t
    (** [for_repo ~user ~repo ()] is a stream of all releases in repo
        [user]/[repo]. *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> id:int64 ->
      unit -> Github_t.release Response.t Monad.t
    (** [get ~user ~repo ~id ()] is release number [id] in repo
        [user]/[repo]. *)

    val get_by_tag_name:
      ?token:Token.t ->
      user:string -> repo:string -> tag:string ->
      unit -> Github_t.release Monad.t
    (** [get_by_tag_name ~user ~repo ~tag ()] is the release in repo
        [user]/[repo] which is using git tag [tag]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.new_release ->
      unit -> Github_t.release Response.t Monad.t
    (** [create ~user ~repo ~release ()] is the newly created release
        described by [release] in repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> id:int64 -> unit -> unit Response.t Monad.t
    (** [delete ~user ~repo ~id ()] activates after release [id]
        in repo [user]/[repo] has been deleted. *)

    val update :
      ?token:Token.t ->
      user:string -> repo:string ->
      release:Github_t.update_release -> id:int64 ->
      unit -> Github_t.release Response.t Monad.t
    (** [update ~user ~repo ~release ~id ()] is the updated release
        [id] in [user]/[repo] as described by [release]. *)

    val upload_asset :
      ?token:Token.t ->
      user:string -> repo:string ->
      id:int64 -> filename:string -> content_type:string ->
      body:string ->
      unit -> unit Response.t Monad.t
    (** [upload_asset ~user ~repo ~id ~filename ~content_type ~body ()]
        activates after [body] is uploaded to repo [user]/[repo] as
        an asset for release [id] with file name [filename] and content
        type [content_type]. *)
  end

  (** The [Deploy_key] module provides the means to manage
      per-repository
      {{:https://docs.github.com/developers/overview/managing-deploy-keys#deploy-keys}
      deploy keys}.
      @see <https://docs.github.com/rest/reference/repos#deploy-keys> deploy key
      API docs
  *)
  module Deploy_key : sig
    val for_repo:
      ?token:Token.t ->
      user:string -> repo:string -> unit -> Github_t.deploy_key Stream.t
    (** [for_repo ~user ~repo ()] is a stream of deploy keys
        associated with repo [user]/[repo]. *)

    val get:
      ?token:Token.t ->
      user:string -> repo:string -> id:int64 ->
      unit -> Github_t.deploy_key Response.t Monad.t
    (** [get ~user ~repo ~id ()] is deploy key [id] for repo [user]/[repo]. *)

    val create :
      ?token:Token.t ->
      user:string -> repo:string ->
      new_key:Github_t.new_deploy_key ->
      unit -> Github_t.deploy_key Response.t Monad.t
    (** [create ~user ~repo ~new_key ()] is the newly created deploy
        key [new_key] for repo [user]/[repo]. *)

    val delete:
      ?token:Token.t ->
      user:string -> repo:string -> id:int64 ->
      unit -> unit Response.t Monad.t
    (** [delete ~user ~repo ~id ()] activates after deploy key
        [id] in repo [user]/[repo] has been deleted. *)
  end

  (** The [Gist] module provides access to the GitHub
      {{:https://docs.github.com/rest/reference/gists}gist API}. *)
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
    (** [all_public ?since ()] is a stream of all of the public gists for the
        current token's user or all public gists if invoked without a current
        token. If [?since] is an ISO 8601 timestamp, only gists updated since
        this time are returned. *)

    val starred :
      ?token:Token.t ->
      ?since:string -> unit -> Github_t.gist Stream.t
    (** [starred ?since ()] is a stream of all starred gists for the
        current token's user. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. *)

    val get :
      ?token:Token.t ->
      id:string -> unit -> Github_t.gist Response.t Monad.t
    (** [get ~id ()] is the gist [id]. *)

    val create :
      ?token:Token.t ->
      gist:Github_t.new_gist -> unit -> Github_t.gist Response.t Monad.t
    (** [create ~gist ()] is a newly created gist described by [gist]. *)

    val update :
      ?token:Token.t ->
      id:string -> gist:Github_t.update_gist ->
      unit -> Github_t.gist Response.t Monad.t
    (** [update ~id ~gist ()] is the updated gist [id] as described
        by [gist]. *)

    val commits :
      ?token:Token.t ->
      id:string -> unit -> Github_t.gist_commit Stream.t
    (** [commits ~id ()] is a stream of commits for gist [id]. *)

    val star :
      ?token:Token.t ->
      id:string -> unit -> unit Response.t Monad.t
    (** [star ~id ()] activates after gist [id] is marked as
        starred by the current token's user. *)

    val unstar :
      ?token:Token.t ->
      id:string -> unit -> unit Response.t Monad.t
    (** [unstar ~id ()] activates after gist [id] is marked as
        not starred by the current token's user. *)

    (* is_starred *)

    val fork :
      ?token:Token.t ->
      id:string -> unit -> Github_t.gist Response.t Monad.t
    (** [fork ~id ()] is a newly forked gist from gist [id]. *)

    val forks :
      ?token:Token.t ->
      id:string -> unit -> Github_t.gist_fork Stream.t
    (** [forks ~id ()] is a stream of forks of gist [id]. *)

    val delete :
      ?token:Token.t ->
      id:string -> unit -> unit Response.t Monad.t
    (** [delete ~id ()] activates after gist [id] has been deleted. *)
  end

  (** The [Emoji] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/emojis}emoji API}. *)
  module Emoji : sig
    val list : ?token:Token.t -> unit -> Github_t.emojis Response.t Monad.t
    (** [list ()] is the list of all available emojis for use on
        GitHub in GitHub-flavored markdown. *)

  end

  (** The [Check] module exposes Github's
      {{:https://docs.github.com/en/rest/reference/checks}checks API}. *)
  module Check : sig
    val create_check_run :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      body:string -> (* JSON type here for body*)
      unit -> Github_t.check_run Response.t Monad.t
    (** [create_check_run ~owner ~repo ~body] creates a new check run for a specified commit in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#create-a-check-run}create-a-check-run}.
    *)

    val update_check_run :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_run_id:string ->
      body:string -> (* JSON type here for body*)
      unit -> Github_t.check_run Response.t Monad.t
    (** [update_check_run ~owner ~repo ~check_run_id ~body] for a specified [check_run_id] in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#update-a-check-run}update-a-check-run}.
    *)

    val get_check_run :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_run_id:string ->
      unit -> Github_t.check_run Response.t Monad.t 
    (** [get_check_run ~owner ~repo ~check_run_id] using its [check_run_id] in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#get-a-check-run}get-a-check-run}.
    *)

    val list_annotations :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_run_id:string ->
      unit -> Github_t.check_run_annotations Response.t Monad.t 
    (** [list_annotations ~owner ~repo ~check_run_id] for a check run using the annotation [check_run_id].

        See {{:https://docs.github.com/en/rest/reference/checks#list-check-run-annotations}list-check-run-annotations}.
    *)    
    
    val list_check_runs :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_suite_id:string ->
      unit -> Github_t.check_runs_list Response.t Monad.t
    (** [list_check_runs ~owner ~repo ~check_suite_id] in a check suite using its [check_suite_id].
        
        See {{:https://docs.github.com/en/rest/reference/checks#list-check-runs-in-a-check-suite} list-check-runs-in-a-check-suite}.
    *)    
    
    val list_check_runs_for_ref :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      sha:string ->
      ?check_name:string ->
      ?app_id:string ->
      ?status:string ->
      unit -> Github_t.check_runs_list Response.t Monad.t
    (** [list_check_runs_for_ref ~owner ~repo ~sha], the [sha] can be a SHA, branch name, or a tag name. 

          See {{:https://docs.github.com/en/rest/reference/checks#list-check-runs-for-a-git-reference}list-check-runs-for-a-git-reference}.
    *)

    val create_check_suite :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      body:string ->
      unit -> Github_t.check_suite Response.t Monad.t
    (** [create_check_suite ~owner ~repo ~body] where body is the sha of the head commit.

        See {{:https://docs.github.com/en/rest/reference/checks#create-a-check-suite}create-a-check-suite}.
     *)

    val update_preferences_for_check_suites :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      body:string ->
      unit -> Github_t.check_suite_preferences Response.t Monad.t
    (** [update_preferences_for_check_suites ~owner ~repo ~body] changes the default automatic flow when creating check suites.
        Where [body] contains an array of auto_trigger_checks

        See {{:https://docs.github.com/en/rest/reference/checks#update-repository-preferences-for-check-suites}update-repository-preferences-for-check-suites}.
     *)

    val get_check_suite :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_suite_id:int ->
      unit -> Github_t.check_suite Response.t Monad.t
    (** [get_check_suite ~owner ~repo ~check_suite_id] retrieves a single check_suite using its [check_suite_id]
        
        See {{:https://docs.github.com/en/rest/reference/checks#get-a-check-suite}get-a-check-suite}.
     *)

    val rerequest_check_suite :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      check_suite_id:int ->
      unit -> unit Response.t Monad.t
    (** [rerequest_check_suite ~owner ~repo ~check_suite_id] triggers GitHub to rerequest an existing check suite, 
        without pushing new code to a repository. 

        See {{:https://docs.github.com/en/rest/reference/checks#rerequest-a-check-suite}rerequest-a-check-suite}.
     *)

    val list_check_suites_for_ref :
      ?token:Token.t ->
      owner:string ->
      repo:string ->
      sha:string ->
      unit -> Github_t.check_suite_list Response.t Monad.t
    (** [list_check_suites_for_ref ~owner ~repo ~sha] lists check suites for a commit [sha]. 

     See {{:https://docs.github.com/en/rest/reference/checks#list-check-suites-for-a-git-reference}list-check-suites-for-a-git-reference}.
     *)
  end

  (** The [Search] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/search}search interfaces}. *)
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

     val issues :
       ?token:Token.t ->
       ?sort:Filter.repo_sort ->
       ?direction:Filter.direction ->
       qualifiers:Filter.issue_qualifier list ->
       keywords:string list ->
       unit -> Github_t.repository_issue_search Stream.t
    (** [issues ?sort ?direction ~qualifiers ~keywords ()] is a
        stream of issue search results for [keywords] and
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
module type Env = sig
  val debug : bool
  (** [debug] is the initial debugging flag value. *)
end

(** A module of this type is required in order to construct a
    {!Github} module using {!Github_core.Make}. *)
module type Time = sig
  val now : unit -> float
  (** [now ()] is the current UNIX epoch time in seconds. *)

  val sleep : float -> unit Lwt.t
  (** [sleep sec] activates after [sec] seconds have elapsed. *)
end
