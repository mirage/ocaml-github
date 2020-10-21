## ocaml-github: GitHub APIv3 OCaml Library

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/mirage/ocaml-github/master&logo=ocaml)](https://ci.ocamllabs.io/github/mirage/ocaml-github/)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/ocaml-github/)

This library provides an OCaml interface to the [GitHub
APIv3](https://docs.github.com/rest/) (JSON). It is compatible with
[MirageOS](https://mirage.io) and also compiles to pure JavaScript via
[js_of_ocaml](http://ocsigen.org/js_of_ocaml).

It is [not yet complete](#api-support-coverage) but
[lib/github.atd](https://github.com/mirage/ocaml-github/blob/master/lib/github.atd)
contains the data types that have been bound so far.

There are several tests and examples in
[lib_test](https://github.com/mirage/ocaml-github/tree/master/lib_test)
for small bits of
functionality. [jar](https://github.com/mirage/ocaml-github/tree/master/jar)
contains utility programs that use the [git jar](#git-jar) facility for
stored tokens.

If you are interested in easily using this library to listen for GitHub
web hook events, you should look at [dsheets/ocaml-github-hooks](https://github.com/dsheets/ocaml-github-hooks).

## Debugging

Two environment variables will cause more debugging to be output:

    GITHUB_DEBUG=1   # API calls output to stderr
    COHTTP_DEBUG=1   # even more HTTP-level debugging

If using the bindings from the toplevel, you can also set `Github.log_active`
to `true` to get the same effect as setting the `GITHUB_DEBUG` environment
variable.

## `git jar`

Applications that use this library will need to save authorization
tokens locally, and the `Github_cookie_jar` module in
[unix](https://github.com/mirage/ocaml-github/tree/master/unix) helps
handle this more naturally.  It maps local application name to an
authorization token so that the application can query the cookie jar at
runtime and use the resulting token in Github API calls.

The tokens are all stored in `$HOME/.github/jar/<name>`, where `<name>` is the
local name of the application.

A `git-jar` command will be installed to add, remove, and list the contents
of this cookie jar.

```console
$ git jar
```

...will display the man page.


```console
$ git jar make avsm rwo
Enter Github password: **********
Enter 2FA code from 'app': 172217
Github cookie jar: created /home/avsm/.github/jar/rwo
Created token rwo (236241): <token>
```

```console
$ git jar show avsm
Enter Github password: **********
Enter 2FA code from 'app': 001221
Cookie Name | ID       | Application                              | Note
----------------------------------------------------------------------------------
        rwo | 236241   | Real World OCaml (API)                   |
   <remote> | 340988   | Travis                                   |
```

Your Github application can now use it via the `Github_cookie_jar` module:

```ocaml
# #require "github.unix";;
# Github_cookie_jar.(init () |> Lwt_main.run |> get ~name:"rwo");;
- : Github_t.auth option =
Some
 {Github_t.auth_scopes = [`Public_repo];
  auth_token = "<token>";
  auth_app =
   {Github_t.app_name = "Real World OCaml";
    app_url = "https://docs.github.com/rest/reference/oauth-authorizations"};
  auth_url = "https://api.github.com/authorizations/236241";
  auth_id = 236241; auth_note = Some "rwo"; auth_note_url = None}
```

## Manipulate GitHub releases

The [Releases](https://docs.github.com/rest/reference/repos#releases) API in
GitHub cannot itself be synched via Git, so this command-line tool lets you
specify a source user/repo and destination user/repo pair, and copies all the
releases from one to the other.

The `git-sync-releases` binary can copy all the releases from one
repository to another for you.

```
$ git sync-releases mirage ocaml-uri avsm ocaml-uri
```

You can also associate binary files with any release, for example to
include pregenerated build files.  The `git upload-release` binary
will do this for you.

```
$ git upload-release mirage ocaml-uri v1.4.0 release.tar.gz
```

## API support coverage

### [Media Types](https://docs.github.com/rest/overview/media-types)

*Supported*: application/vnd.github.v3+json

*Not yet supported*: Other media types

### [OAuth](https://docs.github.com/developers/apps/authorizing-oauth-apps)
*Supported*:

 * Web and non-Web flows with two-factor authentication
 * Basic Authorizations API

*Not yet supported*:

 * [Check](https://docs.github.com/rest/reference/oauth-authorizations) (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * [Reset](https://docs.github.com/rest/reference/oauth-authorizations) (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * Fingerprint retrieval (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * get-or-create, update, revoke
 * fingerprint endpoints

### [Activity](https://docs.github.com/rest/reference/activity)
*Supported*:

 * All [Events](https://docs.github.com/rest/reference/activity#events) endpoints
 * Event types: commit comment, create, delete, deployment, deployment status,
   download, follow, fork, fork apply, gist, gollum, issue comment,
   issues, member, page build, public, pull request, pull request review
   comment, push, release, repository, status, team add, watch

*Not yet supported*:

 * Event types: membership
 * [Feeds](https://docs.github.com/rest/reference/activity#feeds)
 * [Notifications](https://docs.github.com/rest/reference/activity#notifications)
 * [Starring](https://docs.github.com/rest/reference/activity#starring)
 * [Watching](https://docs.github.com/rest/reference/activity#watching)

### [Gists](https://docs.github.com/rest/reference/gists)
*Supported*:

 * All endpoints

*Not yet supported*:

 * Special media types
 * Truncation helpers

### [Git Data](https://docs.github.com/rest/reference/git)

*Not yet supported*: everything (see
 [#40](https://github.com/mirage/ocaml-github/issues/40))

### [Issues](https://docs.github.com/rest/reference/issues)
*Supported*:

 * All basic endpoints
 * Basic comments endpoints
 * [Milestones](https://docs.github.com/rest/reference/issues#milestones)
 * [Labels](https://docs.github.com/rest/reference/issues#labels)
 * [Repository issue comments](https://docs.github.com/rest/reference/issues#list-issue-comments-for-a-repository)
 * [Get a single issue comment](https://docs.github.com/rest/reference/issues#get-an-issue-comment)
 * [Edit an issue comment](https://docs.github.com/rest/reference/issues#update-an-issue-comment) (see [#87](https://github.com/mirage/ocaml-github/issues/87))
 * [Delete an issue comment](https://docs.github.com/rest/reference/issues#delete-an-issue-comment)
 * [Issue events](https://docs.github.com/rest/reference/issues#events)
 * [Timeline](https://docs.github.com/rest/reference/issues#timeline)

*Not yet supported*:

 * Custom media types
 * [Assignees](https://docs.github.com/rest/reference/issues#assignees)

### Miscellaneous
*Supported*:

 * [Rate limit](https://docs.github.com/rest/reference/rate-limit)
 * [Emojis](https://docs.github.com/rest/reference/emojis)

*Not yet supported*:

 * [Gitignore](https://docs.github.com/rest/reference/gitignore)
 * [Markdown](https://docs.github.com/rest/reference/markdown)
 * [Meta](https://docs.github.com/rest/reference/meta)
 * [Licenses](https://docs.github.com/rest/reference/licenses)

### [Organizations](https://docs.github.com/rest/reference/orgs)
*Supported*:

 * [List teams](https://docs.github.com/rest/reference/teams#list-teams)
 * [Get team](https://docs.github.com/rest/reference/teams#get-a-team-by-name)
 * [List team repos](https://docs.github.com/rest/reference/teams#list-team-repositories)
 * [List your organizations](https://docs.github.com/rest/reference/orgs#list-organizations-for-the-authenticated-user)
 * [List (public) user organizations](https://docs.github.com/rest/reference/orgs#list-organizations-for-a-user)
 * [Webhooks](https://docs.github.com/rest/reference/orgs#webhooks)

*Not yet supported*: everything else

### [Pull Requests](https://docs.github.com/rest/reference/pulls)
*Supported*:

 * All endpoints

*Not yet supported*:

 * Link relations
 * Custom media types

### [Repositories](https://docs.github.com/rest/reference/repos)
*Supported*:

 * [Create](https://docs.github.com/rest/reference/repos#create-a-repository-for-the-authenticated-user)
 * [List user
   repositories](https://docs.github.com/rest/reference/repos#list-repositories-for-a-user)
 * [Get](https://docs.github.com/rest/reference/repos#get-a-repository)
 * [Delete repository](https://docs.github.com/rest/reference/repos#delete-a-repository)
 * [List tags](https://docs.github.com/rest/reference/repos#list-repository-tags)
 * [List branches](https://docs.github.com/rest/reference/repos#list-branches)
 * [Get a single
   commit](https://docs.github.com/rest/reference/repos#get-a-commit)
 * [Deploy keys](https://docs.github.com/rest/reference/repos#deploy-keys)
 * [Forks](https://docs.github.com/rest/reference/repos#forks)
 * Most [Releases](https://docs.github.com/rest/reference/repos#releases) endpoints
 * [Create a
   status](https://docs.github.com/rest/reference/repos#create-a-commit-status)
 * [List statuses for a specific
   ref](https://docs.github.com/rest/reference/repos#list-commit-statuses-for-a-reference)
 * [Get the combined status for a specific
   ref](https://docs.github.com/rest/reference/repos#get-the-combined-status-for-a-specific-reference)
 * [List contributors](https://docs.github.com/rest/reference/repos#list-repository-contributors)
 * Most [Webhooks](https://docs.github.com/rest/reference/repos#webhooks) endpoints
 * [Get contributors list with additions, deletions, and commit counts](https://docs.github.com/rest/reference/repos#get-all-contributor-commit-activity)
 * [Collaborators](https://docs.github.com/rest/reference/repos#collaborators)
 * [List organization
   repositories](https://docs.github.com/rest/reference/repos#list-organization-repositories)
 * [Get the last year of commit activity
   data](https://docs.github.com/rest/reference/repos#get-the-last-year-of-commit-activity) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the number of additions and deletions per week](https://docs.github.com/rest/reference/repos#get-the-weekly-commit-activity) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the weekly commit count for the repository owner and everyone else](https://docs.github.com/rest/reference/repos#get-the-weekly-commit-count) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the number of commits per hour in each day](https://docs.github.com/rest/reference/repos#get-the-hourly-commit-count-for-each-day) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the latest
   release](https://docs.github.com/rest/reference/repos#get-the-latest-release)
 * [List assets for a
   release](https://docs.github.com/rest/reference/repos#list-release-assets)
 * [Get a release by tag name](https://docs.github.com/rest/reference/repos#get-a-release-by-tag-name)
 * [Get a single release
   asset](https://docs.github.com/rest/reference/repos#get-a-release-asset)
 * [Delete a release
   asset](https://docs.github.com/rest/reference/repos#delete-a-release-asset)

*Not yet supported*:

 * [List your
   repositories](https://docs.github.com/rest/reference/repos#list-repositories-for-the-authenticated-user)
 * [List all public
   repositories](https://docs.github.com/rest/reference/repos#list-public-repositories)
 * [Edit](https://docs.github.com/rest/reference/repos#update-a-repository)
 * [List
   languages](https://docs.github.com/rest/reference/repos#list-repository-languages)
 * [List teams](https://docs.github.com/rest/reference/repos#list-repository-teams)
 * [Get branch](https://docs.github.com/rest/reference/repos#get-a-branch)
 * [Commit comments](https://docs.github.com/rest/reference/repos#comments)
 * [List
   commits](https://docs.github.com/rest/reference/repos#list-commits)
 * [Compare two
   commits](https://docs.github.com/rest/reference/repos#compare-two-commits)
 * [Contents](https://docs.github.com/rest/reference/repos#contents)
 * [Deployments](https://docs.github.com/rest/reference/repos#deployments)
 * [Merging](https://docs.github.com/rest/reference/repos#merging)
 * [Pages](https://docs.github.com/rest/reference/repos#pages)
 * [Get the latest
   release](https://docs.github.com/rest/reference/repos#get-the-latest-release)
 * [Edit a release
   asset](https://docs.github.com/rest/reference/repos#update-a-release-asset)
 * [Ping a
   hook](https://docs.github.com/rest/reference/repos#ping-a-repository-webhook)
 * [PubSubHubbub](https://docs.github.com/rest/reference/repos#pubsubhubbub)
 * [Receiving Webhooks
   helpers](https://docs.github.com/rest/reference/repos#receiving-webhooks)

### [Search](https://docs.github.com/rest/reference/search)
*Supported*:

 * [Search
   repositories](https://docs.github.com/rest/reference/search#search-repositories)

*Not yet supported*:

 * [Search code](https://docs.github.com/rest/reference/search#search-code)
 * [Search issues](https://docs.github.com/rest/reference/search#search-issues-and-pull-requests)
 * [Search users](https://docs.github.com/rest/reference/search#search-users)
 * [Text match media
    type](https://docs.github.com/rest/reference/search#text-match-metadata)

### [Users](https://docs.github.com/rest/reference/users)
*Supported*:

 * [Get a single
   user](https://docs.github.com/rest/reference/users#get-a-user)
 * [Get the authenticated
   user](https://docs.github.com/rest/reference/users#get-the-authenticated-user)

*Not yet supported*:

 * [Update the authenticated
   user](https://docs.github.com/rest/reference/users#update-the-authenticated-user)
 * [Get all users](https://docs.github.com/rest/reference/users#list-users)

### [Enterprise](https://docs.github.com/rest/reference/enterprise-admin)
*Not yet supported*: everything
