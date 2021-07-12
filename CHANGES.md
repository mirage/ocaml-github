## dev

- Fixes to odoc warnings and cohttp dependencies (@Aaylor #244)
- Support for 4.12 and fixing recent compiler warnings (@tmcgilchrist #246 and @emillon #250 #247)
- Add a new package `github-data` which contains just the serialisation logic
  without a dependency on the web stack (#248 @emillon)
- Add Github checks API support (#249 @tmcgilchrist)

## 4.3.2 (2020-09-21)

- Fix authentication on POST/PATCH/PUT requests. (#242 @Aaylor)
- Add support for statistics endpoint. (#240 @tmcgilchrist)
- Add support for listing organization's repository. (#239 @tmcgilchrist)
- Remove the dependency `lambda-term`, which was only used to read password, for
  the package `github-unix`. (#238 @emillon)
- Add the field `committer` in the datatype `git_commit`. (#235 @Aaylor)

## 4.3.1 (2020-08-18)

- Fix a bug introduced by #228, by adding a default value when `user_type` is
  not defined (#232 @Aaylor)
- Do not print errors on `stderr`. (#234 @emillon)

## 4.3.0 (2020-07-20)

- Remove deprecated authentication method as GitHub has removed
  support for it (#230 @Aaylor)
- Reintroduce `user_type` to distinguish organisations and
  users (#228 @Aaylor)

## 4.2.0 (2019-06-17)

- Add repository permissions support (#226 @Aaylor)
- Regenerate opam files automatically via dune-project (#227 @avsm)

## 4.1.0 (2019-06-03)

- Add the interface for `/user/orgs` (#222 @Aaylor)
- Switch to dune-release instead of topkg (#224 @avsm)
- Do not use deprecated `Yojson.Safe.json` (#224 @avsm)
- Support lambda-term/zed 2.0.0 interfaces (#224 @avsm)
- Use wrapped `js_of_ocaml` 3.4.0 interfaces (@avsm)

## 4.0.0 (2018-12-11)

- Port to latest Atd 2.0.0 interfaces (#218 by @mjambon and @avsm)
- Port build system to Dune (@avsm)
- Properly expose the GitHub JavaScript library and fix dependencies (#216 by @samoht)
- Add `pull.merge_commit_sha` (#217 from @AltGr)
- Convert local opam files to the 2.0 format. (@avsm)

## 3.1.0 (2018-02-14)

- Make contributor stats author field nullable (#211, @rvantonder)
- Support OCaml 4.06 by enabling `-safe-string` compatibility (#212, @jpdeplaix)
- Add support for repository issue search (#196, @avsm and @samoht)
- TLS isn't a hard dependency of githug (#208, @rgrinberg)

## 3.0.1 (2017-08-01):
* Update to work with latest cohttp (#205 from @rgrinberg)
* Fix atdgen JSON codec generation bug in 3.0.0 (#205)
* Remove deprecated Hook module (#206)

## 3.0.0 (2017-07-03):

Port to Jbuilder (#202 by @rgrinberg @dsheets @samoht). This
splits up the `opam` packages into three separate ones:

- `github`: the `github_s`, `github_core`, `github_j` and `github_t` modules.
- `github-unix`: the `Github` and `Github_cookie_jar` modules.
- `github-jsoo`: the js_of_ocaml `Github_js` module.

Tools that depended on github-unix previously will now need to
adjust their `opam` files to depend on the `github-[unix|jsoo]`
packages, and should also to rename `github.unix` to `github-unix`
and `github.js` to `github-jsoo`.  However, transitional packages
are available for the older findlib names, so you only need to
immediately rename your `opam` files for the moment.

* Minimum supported OCaml version is now 4.03.0 or higher.

## 2.3.0 (2017-04-13):
* Changes marked with ! are type changes
* ! repo_issue_event has changed the actor type to a linked_user option
  since GitHub sometimes returns a null response.
* Fix various test cases that were not compiling due to API changes.
* Add Issue.timeline_events
* Add Organization.Hook
* Expose Hook as Repo.Hook and deprecate Hook
* Add Repo.Hook.parse_event_metadata
* Add an optional `media_type` argument to API.get and API.get_stream

## 2.2.0 (2016-12-10):
* Add Repo.get_ref (#175 from @samoht)
* Add Endpoint.Version.t
* Add Stream.since
* Add Stream.version
* Add Monad.catch
* Add Monad.fail

## 2.1.0 (2016-11-03):
* Changes marked with ! are type changes
* ! push_event_hook_head_commit field added

## 2.0.3 (2016-09-30):
* Changes marked with ! are type changes
* ! repo_issue_event_label and repo_issues_event_label have changed type
  from label option to base_label option as the GitHub APIs for Issue
  Events and Issue Labels are not consistent with the inclusion of the
  url field
* base_label type added (label without the url field)

## 2.0.2 (2016-09-26):
* Changes marked with ! are type changes
* ! Issue.remove_label now returns a label list Response.t Monad.t like
  the other issue label modification functions because GitHub does not
  treat this DELETE endpoint like others and return 204 No Content. The
  GitHub docs are wrong on this point (a support ticket has been filed
  as they have now moved their developer API docs into a closed source
  repo). Because any previous user of the API would have immediately
  encountered this exception, this is a patch release.

## 2.0.1 (2016-09-23):
* Changes marked with ! are type changes
* ! web_hook_config_content_type field is now optional because GitHub
  does not appear to byte-wise validate the field and some web hook
  users (e.g. CircleCI) set a `content-type` field rather than a
  `content_type` field.

## 2.0.0 (2016-09-21):
* Changes marked with ! are type changes
* ! Fix Event.for_repo_issues and rename to Issue.events_for_repo
  (#107 from @yallop)
* update_issue type added
* ! Change Issue.update to accept update_issue rather than new_issue
  This allows users to change issue state (open/close).
* ! event_type now has Repository constructor
* ! event_type now has All constructor
* ! event_type now has Unknown fall-back constructor
* ! event_constr now has Repository constructor
* ! scope variant now has Unknown fall-back constructor
* ! issue_sort variant now has Unknown fall-back constructor
* ! team_permission variant now has Unknown fall-back constructor
* ! wiki_page_action variant now has Unknown fall-back constructor
* ! issue_comment_action variant now has Edited constructor
* ! issue_comment_action variant now has Deleted constructor
* ! issue_comment_action variant now has Unknown fall-back constructor
* ! issues_action variant now has Edited constructor
* ! issues_action variant now has Unknown fall-back constructor
* ! member_action variant now has Unknown fall-back constructor
* ! page_build_status variant now has Unknown fall-back constructor
* ! pull_request_action variant now has Edited constructor
* ! pull_request_action variant now has Unknown fall-back constructor
* ! pull_request_review_comment_action variant now has Edited constructor
* ! pull_request_review_comment_action variant now has Deleted constructor
* ! pull_request_review_comment_action variant now has Unknown fall-back
  constructor
* ! release_action variant now has Unknown fall-back constructor
* ! watch_action variant now has Unknown fall-back constructor
* ! status_state variant now has Unknown fall-back constructor
* ! update_pull_base field added (PR target branch is now updatable)
* ! URI.repo_issues was removed
* ! URI.repo_issue was removed
* ! URI.repo_pulls was removed
* ! URI.repo_milestones was removed
* ! URI.repo_contributors_stats was removed
* ! URI.issue_comments was removed
* ! URI.issue_comment was removed
* ! URI.milestone was removed
* ! Issue.comments signature changed
* Add Labels API support (#146 from @dave-tucker)
* Add Collaborators API support
* Add Emojis API support
* Make `tls` the default recommended dependency instead of OpenSSL
* Require atdgen >= 1.10.0 for <json untyped> support
* Add HTTP redirect support (Response.redirect(s) and Response.final_resource)
* Improve GitHub error reporting (#151)
* Add Organization.user_orgs (#130 from @yallop)
* Add Hook.parse_event
* Add event_hook_constr type for the web hook event variants
* Add Repo.create
* Add Repo.delete
* Add Stream.fold
* Add Issue.get
* Add Status.get
* Add Issue.comments_for_repo
* Add Issue.get_comment
* Add Issue.update_comment
* Add Issue.delete_comment
* Add repository_event
* Add type Filter.issue_comment_sort
* Add Issue.events
* push_event_before field added

## 1.1.0 (2016-06-20):
* Add new_status_context and status_context fields (#88)
* Add setting the jar cookie by the `GH_COOKIE` env var (#100 by @rgrinberg)
* Remove camlp4 as a build time dependency (#99, #104, #106 by @rgrinberg)
* Add Windows tests via Appveyor (#98)
* Add jar 'local' subcommand for printing local cookies (#111 by @rgrinberg)
* Add Repo.contributor_stats for contributor statistics (#114 by @sevenEng)
* Add stats_contributor type (#114 by @sevenEng)
* Add stats_contributors type (#114 by @sevenEng)
* Add contribution_week type
* Fix Repo.get_tags_and_times exception when repository has no tags (#113)
* Change Github_core.Make to accept an Env module making the library
  Mirage compatible by moving a Unix.getenv invocation into a parameter (#93)
* Add contributor and contributors types (#112)
* Add Repo.contributors to list contributors to a repository (#112)
* Register automatically a Message exception printer (#116)
* Fix `git jar` help strings to match the command reality.
* Improve `git jar create --help` manual page.
* Add `git-gist create [--public] --descr <descr> <file1> <fileN>` to
  upload new gists.

## 1.0.0 (2015-06-01):
* Changes marked with ! are type changes (not including field additions)
* ! Monad.bind now accepts a function first and then a Monad.t
* ! API.{get,post,delete,patch,put} now take optional fail_handlers
* ! API.{get,post,delete,patch,put} now take optional rate classification
* ! API.{get,post,delete,patch,put} now return 'a Response.t Monad.t
* ! URI.authorize now requires ~state argument to protect against CSRF.
* ! URI.repo_issue ~issue_number argument was renamed to ~num
* ! URI.issue_comments ~issue_number argument was renamed to ~num
* ! URI.issue_comment ~commit_id argument was renamed to ~num
* ! Scope.scope_of_string was renamed Scope.of_string
* ! Scope.string_of_scope was renamed Scope.to_string
* ! Scope.scopes_of_string was renamed Scope.list_of_string
* ! Scope.list_of_string returns a scope list option for unparseable scopes
* ! Scope.string_of_scopes was renamed Scope.list_to_string
* ! Token.{create,get_all,get,delete} return _ authorization Response.t Monad.t
* ! Token.{create,get_all,get,delete} now have additional ?otp:string argument
* ! Token.create now has additional ?fingerprint:string argument
* ! Token.get now returns auth option authorization Response.t Monad.t
* ! Token.{get,delete} ~num argument was renamed to ~id and changed to int64
* ! User.current_info ~token argument was made optional ?token
* ! User.current_info now returns user_info Response.t Monad.t
* ! User.info ~login argument was renamed to ~user
* ! User.info now returns user_info Response.t Monad.t
* ! User.repos is now User.repositories
* ! User.repositories now accepts optional ?token and does not accept ?page
* ! Pull.for_repo does not accept ?page and now returns pull Stream.t
* ! Pull.list_commits was renamed Pull.commits and now returns commit Stream.t
* ! Pull.list_files was renamed Pull.files and now returns file Stream.t
* ! Pull.get now returns pull Response.t Monad.t
* ! Pull.create now returns pull Response.t Monad.t
* ! Pull.create_from_issue now returns pull Response.t Monad.t
* ! Pull.update now returns pull Response.t Monad.t
* ! Pull.is_merged now returns bool Response.t Monad.t
* ! Pull.merge now returns merge Response.t Monad.t
* ! Milestone.get now returns milestone Response.t Monad.t
* ! Milestone.create now returns milestone Response.t Monad.t
* ! Milestone.update now returns milestone Response.t Monad.t
* ! Milestone.delete now returns unit Response.t Monad.t
* ! Milestone.for_repo does not accept ?page
* ! Milestone.for_repo now returns milestone Stream.t
* ! Release.get now returns release Response.t Monad.t
* ! Release.get ~num argument was renamed to ~id and changed to int64
* ! Release.create now returns release Response.t Monad.t
* ! Release.update now returns release Response.t Monad.t
* ! Release.update ~num argument was renamed to ~id and changed to int64
* ! Release.delete now returns unit Response.t Monad.t
* ! Release.delete ~num argument was renamed to ~id and changed to int64
* ! Release.upload_asset now returns unit Response.t Monad.t
* ! Release.upload_asset ~id argument was changed to int64
* ! Release.for_repo now returns release Stream.t
* ! Deploy_key.get now returns deploy_key Response.t Monad.t
* ! Deploy_key.get ~num argument was renamed to ~id and changed to int64
* ! Deploy_key.create now returns deploy_key Response.t Monad.t
* ! Deploy_key.delete now returns unit Response.t Monad.t
* ! Deploy_key.delete ~num argument was renamed to ~id and changed to int64
* ! Deploy_key.for_repo now returns deploy_key Stream.t
* ! Issue.create now returns issue Response.t Monad.t
* ! Issue.update now returns issue Response.t Monad.t
* ! Issue.create_comment now returns issue_comment Response.t Monad.t
* ! Issue.for_repo does not accept ?page and now returns issue Stream.t
* ! Issue.update ~issue_number argument was renamed to ~num
* ! Issue.comments ~issue_number argument was renamed to ~num
* ! Issue.create_comment ~issue_number argument was renamed to ~num
* ! Issue.comments now returns issue_comment Stream.t
* ! Status.for_sha was renamed Status.for_ref and now returns status Stream.t
* ! Status.for_ref ~sha argument was renamed to ~git_ref
* ! Status.create now returns status Response.t Monad.t
* ! Hook.for_repo now returns hook Stream.t
* ! Hook.get now returns hook Response.t Monad.t
* ! Hook.get ~num argument was renamed to ~id and changed to int64
* ! Hook.create now returns hook Response.t Monad.t
* ! Hook.update now returns hook Response.t Monad.t
* ! Hook.update ~num argument was renamed to ~id and changed to int64
* ! Hook.delete now returns unit Response.t Monad.t
* ! Hook.delete ~num argument was renamed to ~id and changed to int64
* ! Hook.test now returns unit Response.t Monad.t
* ! Hook.test ~num argument was renamed to ~id and changed to int64
* ! Repo.tags now returns repo_tag Stream.t
* ! Repo.branches now returns repo_branch Stream.t
* ! Repo.refs now returns git_ref Stream.t
* ! Tag.get_tags_and_times was renamed Repo.get_tags_and_times
* ! Repo.get_tags_and_times now returns (string * string) Stream.t
* ! Tag.tag was renamed Repo.get_tag
* ! Repo.info now returns repository Response.t Monad.t
* ! Repo.fork now returns repository Response.t Monad.t
* ! Repo.get_tag now returns tag Response.t Monad.t
* ! Repo.commit was renamed Repo.get_commit and returns commit Response.t Monad.t
* ! Gist.list_users was renamed Gist.for_user and now returns gist Stream.t
* ! Gist.list was renamed Gist.all and now returns gist Stream.t
* ! Gist.list_all_public was renamed Gist.all_public and now returns gist Stream.t
* ! Gist.list_starred was renamed Gist.starred
* ! Gist.starred ~token argument was made optional ?token
* ! Gist.starred now returns gist Stream.t
* ! Gist.get ~id argument is now int64
* ! Gist.get now returns gist Response.t Monad.t
* ! Gist.create ~token argument was made optional ?token
* ! Gist.create ~contents argument was renamed to ~gist
* ! Gist.create now returns gist Response.t Monad.t
* ! Gist.edit was renamed Gist.update
* ! Gist.update signature now has s/~token/?token/ s/~contents/~gist/
* ! Gist.update now returns gist Response.t Monad.t
* ! Gist.update ~id argument is now int64
* ! Gist.commits ~id argument is now int64
* ! Gist.commits now returns gist_commit Stream.t
* ! Gist.star ~token argument was made optional ?token
* ! Gist.star ~id argument is now int64
* ! Gist.star now returns unit Response.t Monad.t
* ! Gist.unstar ~token argument was made optional ?token
* ! Gist.unstar ~id argument is now int64
* ! Gist.unstar now returns unit Response.t Monad.t
* ! Gist.fork ~token argument was made optional ?token
* ! Gist.fork ~id argument is now int64
* ! Gist.fork now returns gist Response.t Monad.t
* ! Gist.list_forks was renamed Gist.forks and now returns gist_fork Stream.t
* ! Gist.forks ~id argument is now int64
* ! Gist.delete ~token argument was made optional ?token
* ! Gist.delete ~id argument is now int64
* ! Gist.delete now returns unit Response.t Monad.t
* ! Organization.teams now returns team Stream.t
* ! Team.repos is now Team.repositories
* ! Team.repositories now returns repository Stream.t
* ! Team.repositories ~num argument was renamed to ~id and is now int64
* ! Team.info now returns team_info Response.t Monad.t
* ! Team.info ~num argument was renamed to ~id and is now int64
* ! Git_obj.obj_type_to_string was renamed Git_obj.type_to_string
* ! user:email scope constructor UserEmail has been renamed User_email
* ! user:follow scope constructor UserFollow has been renamed User_follow
* ! scope constructor `Admin_org_hook added
* ! org_id, user_id, organization_id are now int64
* ! team_id, team_info_id, team_add_info_id are now int64
* ! auth_id is now int64
* ! repo type has been renamed repository
* ! repos type has been renamed repositories
* ! repository_forks field renamed to repository_forks_count
* ! repository_watchers field renamed to repository_stargazers_count
* ! repository_open_issues field renamed to repository_open_issues_count
* ! repository_master_branch field renamed to repository_default_branch
* ! repository_description is now optional
* ! repository_id is now int64
* ! comment_id is now int64
* ! release_name is now optional
* ! release_body is now optional
* ! release_id is now int64
* ! new_release_name is now optional
* ! new_release_body is now optional
* ! branch_user is now optional
* ! user_info_ty field removed in favor of specific user and organization types
* ! user_type type has been removed
* ! org_gravatar_id field and all derived fields (e.g. user_gravatar_id) removed
* ! team_info_permission is now a variant type instead of a string
* ! team_info_organization is now an org rather than a user
* ! event_org field is now an org option rather than a user
* ! event_id is now int64
* ! web_hook_config_insecure_ssl is now a bool rather than a string
* ! hook_config is now a hook_config rather than a web_hook_config
* ! hook_id is now int64
* ! new_hook_config is now a hook_config rather than a web_hook_config
* ! update_hook_config is now a hook_config rather than a web_hook_config
* ! deploy_key_id is now int64
* ! status_id is now int64
* ! change_statusdeletions field renamed to change_status_deletions
* ! change_statusadditions field renamed to change_status_additions
* ! change_statustotal field renamed to change_status_total
* ! gist_history type has been renamed gist_commit
* ! gist_historyurl field renamed to gist_commit_url
* ! gist_historyversion field renamed to gist_commit_version
* ! gist_historyuser field renamed to gist_commit_user
* ! gist_historychange_status field renamed to gist_commit_change_status
* ! gist_historycommitted_at field renamed to gist_commit_commited_at
* ! gist_history_list type has been renamed gist_commits
* ! gist_create_content type has been renamed new_gist_content (+ fields)
* ! gist_create_contents type has been renamed new_gist_contents
* ! gist_create type has been renamed new_gist (+ fields)
* ! gist_edit type has been renamed update_gist_file
* ! gist_edit_content field renamed to update_gist_file_content
* ! gist_edit_filename field renamed to update_gist_file_name
* ! gist_edits type has been renamed update_gist (+ fields)
* ! gist_fork_id is now int64
* Atdgen >=1.5.0 and Yojson >=1.2.0 are now required for tag_field support
* Cohttp >=0.17.0 is now required for Link header support
* Monad.map, Monad.(>|=), and Monad.embed : 'a Lwt.t -> 'a Monad.t added
* Github.Response module added for metadata about responses
* Monad.(>>~) added to bind and project a Response.t value
* Add Stream module and API.get_stream function for paginated responses (#46)
* `git-jar save` removed after Authorizations API response changes of 2015-04-20
* `git-jar make` now requires a cookie name and defaults to that for token note
* `git-jar revoke` now accepts either a cookie name or a token ID
* Fix git-jar token file permissions security vulnerability
* git-jar now supports 2FA (#38)
* Github.authorization type added
* Github.rate type added for classifying requests into rate limiting regime
* Github.handler type added
* Add API.code_handler to make creating fail_handlers easier
* Github.Message exception added and raised when GitHub returns an API error
* Add API.string_of_message for human consumption of structured errors
* Add API.get_rate to retrieve possibly cached rate-limit information
* Add API.get_rate_limit to retrieve the possibly cached query quota
* Add API.get_rate_remaining to retrieve the possibly cached query quota remaining
* Add API.get_rate_reset to retrieve the possibly cached quota expiry
* Add Rate_limit module to perform explicit rate limit requests
* Add Scope.max which contains a minimal set of scopes for maximum privilege
* Add Repo.fork to create fork of a repository to the present user or given org
* Add Repo.forks to list the forks of a given repository
* Add Search module to access GitHub's search API
* Filter.forks_sort type added
* Add Event module with a variety of event sources
* A new jar command, git-list-events, has been added to print events for a repo
* A new test binary, parse_events, has been added which downloads and
  parses archive event data
* Fixed bug in Issue.for_repo preventing listing of issues without
  milestone (#49,#53 Michael Grünewald)
* A new jar command, git-list-issues, has been added to print issues for a repo
* A new jar command, git-search, has been added to search GitHub
* Add Gist manipulation command line tool (#48)
* Filter.state now includes `All constructor (#59,#62 Michael Grünewald)
* pull_ref type added from which pull now inherits
* issue_pull_request optional field of pull_ref type added
* Issue.is_issue and Issue.is_pull added to distinguish issues from PRs
* organization type added
* user and organization now inherit from org
* user_info_updated_at field added
* user_info_html_url field added
* issue_created_at, issue_updated_at, issue_closed_at added (#50,#51
  from Michael Grünewald)
* repository_subscribers_count field added (the new "watchers")
* repository_language field added
* repository_has_pages field added
* comment type added
* issue_comment_html_url field added
* commit_comment type added
* commit_comment_event type added
* ref type added
* create_event type added
* delete_event type added
* fork_event type added
* wiki_page_action type added
* wiki_page type added
* gollum_event added
* issue_comment_action type added
* issue_comment_event type added
* issues_action type added
* issues_event type added
* member_action type added
* member_event type added
* page_build_error type added
* page_build_status type added
* page_build type added
* page_build_event type added
* pull_request_action type added
* pull_reqest_event type added
* pull_request_review_comment_action type added
* pull_request_review_comment type added
* pull_request_review_comment_event type added
* push_event_author type added
* push_event_commit type added
* push_event type added
* release_action type added
* release_event type added
* status_branch_commit type added
* status_branch type added
* status_event type added
* team_add_info type added
* team_add_event type added
* watch_action type added
* watch_event type added
* event_constr type added
* event_payload field added
* events type added
* hook_config type added

## 0.9.4 (2014-12-18):
* Add bindings for organisation teams and repositories (#45).
* Use `Bytes` instead of `String` for future `safe-string` support.
* Use the Cohttp 0.14.0 API in the test cases and make them optional
  (activate with `--enable-tests` during configure).
* Add a `--json` option to `git-list-releases` so that it can emit
  the release information in JSON rather than Markdown.

## 0.9.3 (2014-11-28):
* Add `repo_branches` and `branches` query functions (#44 from Jeff Hammerbacher).
* Improve `opam` 1.2 metadata.

## 0.9.2 (2014-11-09):
* Better log error messages (#39).
* Tweak Makefile to build JavaScript version by default if `js_of_ocaml` is installed.

## 0.9.1 (2014-11-04):
* Mark `published_at` and `created_at` fields in Releases to be optional,
  as they may not be set in the case of draft tags.

## 0.9.0 (2014-11-02):
* Add `Jar_cli` module for use by applications that use the Git Jar (#34).
* Add bindings to the Gist APIs for storing text fragments (#36).
* Add a JavaScript port, using Cohttp and js_of_ocaml (#36).
* Build `ocamldoc` HTML documentation by default.

## 0.8.6 (2014-08-10):
* Fix `pull_action_type` `synchronize` tag typo (#33 from Philipp Gesang).
* Add a `git create-release` to create a GitHub release, including binary assets
  (#32 from Markus Mottl).

## 0.8.5 (2014-05-08):
* The `master_branch` field in the `repo` is actually optional, to fix the schema.

## 0.8.4 (2014-04-26):
* Add `git list-releases` to list releases in sorted Markdown format.

## 0.8.3 (2014-04-13):
* Add `git sync-releases` to copy release metadata between GitHub repos.
* Add `git upload-release` to upload a binary file to a GitHub release tag.

## 0.8.2 (2014-04-01):
* Remove use of `Re_str` to add POSIX thread safety.
* Add deployment key support in the `Deploy_key` module.

## 0.8.1 (2014-03-07):
* Sync to latest GitHub scopes API.

## 0.8.0 (2014-03-02):
* Port to cohttp.0.10.x interfaces.
* Make the `note` field in oAuth token creation mandatory to reflect GitHub API.
* Pull requests are now allowed to have `null` bodys (#31).

## 0.7.1 (2014-02-28):
* Log response bodies in the event of an API parsing failure. (#29)
* Expose `log_active` as a reference so it can be used from the toplevel. (#30)
* Add `Github.URI.pull_raw_diff` to point to the location of a pull request diff.

## 0.7.0 (2014-01-03):
* Add a User.repos call to list a users repositories.
* Change repo type such that the field 'pushed_at' is now an option type.
* Accept optional page argument in Pull, Milestone, and Issue.
* Add `UserEmail`, `UserFollow` and `Notifications` scopes.
* Add `Releases` module to handle the release management addition to GitHub.
* Add `GITHUB_DEBUG` environment variable to make debugging output optional.
* Regenerate build files with OASIS 0.4.1.
* OCamldoc improvements for the `GitHub` module.

## 0.6.1 (2013-06-21):
* Abstract `Github_cookie_jar.t` and add `Github_cookie_jar.jar_path` accessor.

## 0.6.0 (2013-05-24):
* Update to the Cohttp-0.9.8 interface.

## 0.5.0 (2013-05-10):
* Force `-j-std` to ATDgen to always use standards-compliant JSON (#11).
* Rename `Github.Issues` to `Github.Issue` to parallel other submodules.
* Rename `Github.Issue.edit` to `Github.Issue.update` to parallel other CRUD interfaces.
* Reorder named parameters to raw API submodule functions.
* Add Pull Request API.
* Add Hook API (generic "web" hooks only).
* Add Statuses API.
* Add structured semantic errors.
* Nows sends partially configurable (via `API.set_user_agent`) User-Agent string.
* Add `API.set_token` to bind an access token for subsequent requests.
* Declare ocaml-re dependency.
* Add anonymous bind operator (>>) to `Github.Monad`.
* Add `Github.Token.delete` for revoking GitHub authorization tokens.
* Add `Github_cookie_jar.delete` for deleting local token cookies.
* Add `revoke` command to git-jar.
* Support GitHub cookie jar names with slashes in.
* Change the signature of `Github_cookie_jar.init` from `... -> unit` to `... -> unit Lwt.t`.

## 0.4.3 (2013-01-14):
* Add filters and sort order parameters for `Issues` and `Milestones` for a repository.

## 0.4.2 (2012-12-29):
* Add a `redirect_uri` option to `URI.authorize`, to permit the redirection URL to be parameterizable.
* Add `User.current_info` and `User.info` to retrieve information about the logged in user, or a public one.
* Add `Issues.edit` to patch an existing issue.
* Correct the type of `Issues.milestone` to be an integer.
* `Issues.labels` is now a `string list`, instead of a `string list option` (with the empty list denoting `None`).

## 0.4.1 (2012-12-27):
* Add `Github.Issues.comments` to retrieve issue comments, and an `issue_comment`
  type in the ATD specification for the returned value.
* Add `Github.Issues.create_comment` to add a new issue comment.
* Expose the `milestone` field for an issue.
* Create a default `lib_test/config.ml` if one doesnt exist (from the template
  in `lib_test/config.ml.in`.

## 0.4.0 (2012-12-25):
* Add a `git-jar` command which provides a convenient command-line interface
  to list, create and save tokens.
* Add a `github.unix` subpackage which provides a `Github_cookie_jar` module
  which saves tokens in `~/.github` for other applications to query if
  they use the Github API.
* Complete the auth API, and rename functions slightly for consistency. We now
  have `Token.get_all` and `Token.get` to retrieve auth information, and
 `Token.create` for constructing them. The API also includes support for adding
  notes and URLs, which are stored on the Github side.

## 0.3.3 (2012-12-18):
* Add `Repo.info` to retrieve repository metadata.

## 0.3.2 (2012-12-14):
* Add ATD descriptions for commits, tags, author info, and repo tags.
* Add API calls to retrieve tags, dates and refs.

## 0.3.1 (2012-10-14):
* Support PREFIX during build for installation prefix.
* Adapt to uri-1.3.3 interface (which now supports multi-value
  queries, as per the RFC).

## 0.3.0 (2012-09-11):
* Initial public release.
