xxx
xxx
xxx

## ocaml-github: GitHub APIv3 OCaml Library

[![Build Status](https://travis-ci.org/mirage/ocaml-github.svg)](https://travis-ci.org/mirage/ocaml-github)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/ocaml-github/)

This library provides an OCaml interface to the [GitHub
APIv3](https://developer.github.com/v3/) (JSON). It is compatible with
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
# Github_cookie_jar.get ~name:"rwo";;
- : Github_t.auth option = Some
  { Github_t.auth_scopes = [`Public_repo];
    Github_t.auth_token = "<token>";
    Github_t.auth_app = {
      Github_t.app_name = "Real World OCaml";
      Github_t.app_url = "http://realworldocaml.org/" };
    Github_t.auth_url = "https://api.github.com/authorizations/236241";
    Github_t.auth_id = 236241; Github_t.auth_note = None;
    Github_t.auth_note_url = None }
```

## Manipulate GitHub releases

The [Releases](https://developer.github.com/v3/repos/releases/) API in
GitHub cannot itself be synched via Git, so this command-line tool lets
you specify a source user/repo and destination user/repo pair, and copies
all the releases from one to the other.

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

### [Media Types](https://developer.github.com/v3/media/)

*Supported*: application/vnd.github.v3+json

*Not yet supported*: Other media types

### [OAuth](https://developer.github.com/v3/oauth/)
*Supported*:

 * Web and non-Web flows with two-factor authentication
 * Basic Authorizations API

*Not yet supported*:

 * [Check](https://developer.github.com/v3/oauth_authorizations/#check-an-authorization) (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * [Reset](https://developer.github.com/v3/oauth_authorizations/#reset-an-authorization) (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * Fingerprint retrieval (see [#83](https://github.com/mirage/ocaml-github/issues/83))
 * get-or-create, update, revoke
 * fingerprint endpoints

### [Activity](https://developer.github.com/v3/activity/)
*Supported*:

 * All [Events](https://developer.github.com/v3/activity/events/) endpoints
 * Event types: commit comment, create, delete, deployment, deployment status,
   download, follow, fork, fork apply, gist, gollum, issue comment,
   issues, member, page build, public, pull request, pull request review
   comment, push, release, repository, status, team add, watch

*Not yet supported*:

 * Event types: membership
 * [Feeds](https://developer.github.com/v3/activity/feeds/)
 * [Notifications](https://developer.github.com/v3/activity/notifications/)
 * [Starring](https://developer.github.com/v3/activity/starring/)
 * [Watching](https://developer.github.com/v3/activity/watching/)

### [Gists](https://developer.github.com/v3/gists/)
*Supported*:

 * All endpoints

*Not yet supported*:

 * Special media types
 * Truncation helpers

### [Git Data](https://developer.github.com/v3/git/)

*Not yet supported*: everything (see
 [#40](https://github.com/mirage/ocaml-github/issues/40))

### [Issues](https://developer.github.com/v3/issues/)
*Supported*:

 * All basic endpoints
 * Basic comments endpoints
 * [Milestones](https://developer.github.com/v3/issues/milestones/)
 * [Labels](https://developer.github.com/v3/issues/labels/)
 * [Repository issue comments](https://developer.github.com/v3/issues/comments/#list-comments-in-a-repository)
 * [Get a single issue comment](https://developer.github.com/v3/issues/comments/#get-a-single-comment)
 * [Edit an issue comment](https://developer.github.com/v3/issues/comments/#edit-a-comment) (see [#87](https://github.com/mirage/ocaml-github/issues/87))
 * [Delete an issue comment](https://developer.github.com/v3/issues/comments/#delete-a-comment)
 * [Issue events](https://developer.github.com/v3/issues/events/)
 * [Timeline](https://developer.github.com/v3/issues/timeline/)

*Not yet supported*:

 * Custom media types
 * [Assignees](https://developer.github.com/v3/issues/assignees/)

### [Miscellaneous](https://developer.github.com/v3/misc/)
*Supported*:

 * [Rate limit](https://developer.github.com/v3/rate_limit/)
 * [Emojis](https://developer.github.com/v3/emojis)

*Not yet supported*:

 * [Gitignore](https://developer.github.com/v3/gitignore)
 * [Markdown](https://developer.github.com/v3/markdown)
 * [Meta](https://developer.github.com/v3/meta)
 * [Licenses](https://developer.github.com/v3/licenses)

### [Organizations](https://developer.github.com/v3/orgs/)
*Supported*:

 * [List teams](https://developer.github.com/v3/orgs/teams/#list-teams)
 * [Get team](https://developer.github.com/v3/orgs/teams/#get-team)
 * [List team repos](https://developer.github.com/v3/orgs/teams/#list-team-repos)
 * [List (public) user organizations](https://developer.github.com/v3/orgs/#list-user-organizations)
 * [Webhooks](https://developer.github.com/v3/orgs/hooks/)

*Not yet supported*: everything else

### [Pull Requests](https://developer.github.com/v3/pulls/)
*Supported*:

 * All endpoints

*Not yet supported*:

 * Link relations
 * Custom media types

### [Repositories](https://developer.github.com/v3/repos/)
*Supported*:

 * [Create](https://developer.github.com/v3/repos/#create)
 * [List user
   repositories](https://developer.github.com/v3/repos/#list-user-repositories)
 * [Get](https://developer.github.com/v3/repos/#get)
 * [Delete repository](https://developer.github.com/v3/repos/#delete-a-repository)
 * [List tags](https://developer.github.com/v3/repos/#list-tags)
 * [List branches](https://developer.github.com/v3/repos/#list-branches)
 * [Get a single
   commit](https://developer.github.com/v3/repos/commits/#get-a-single-commit)
 * [Deploy keys](https://developer.github.com/v3/repos/keys/)
 * [Forks](https://developer.github.com/v3/repos/forks/)
 * Most [Releases](https://developer.github.com/v3/repos/releases/) endpoints
 * [Create a
   status](https://developer.github.com/v3/repos/statuses/#create-a-status)
 * [List statuses for a specific
   ref](https://developer.github.com/v3/repos/statuses/#list-statuses-for-a-specific-ref)
 * [Get the combined status for a specific
   ref](https://developer.github.com/v3/repos/statuses/#get-the-combined-status-for-a-specific-ref)
 * [List contributors](https://developer.github.com/v3/repos/#list-contributors)
 * Most [Webhooks](https://developer.github.com/v3/repos/hooks/) endpoints
 * [Get contributors list with additions, deletions, and commit counts](https://developer.github.com/v3/repos/statistics/#get-contributors-list-with-additions-deletions-and-commit-counts)
 * [Collaborators](https://developer.github.com/v3/repos/collaborators/)

*Not yet supported*:

 * [List your
   repositories](https://developer.github.com/v3/repos/#list-your-repositories)
 * [List organization
   repositories](https://developer.github.com/v3/repos/#list-organization-repositories)
 * [List all public
   repositories](https://developer.github.com/v3/repos/#list-all-public-repositories)
 * [Edit](https://developer.github.com/v3/repos/#edit)
 * [List
   languages](https://developer.github.com/v3/repos/#list-languages)
 * [List teams](https://developer.github.com/v3/repos/#list-teams)
 * [Get branch](https://developer.github.com/v3/repos/#get-branch)
 * [Commit comments](https://developer.github.com/v3/repos/comments/)
 * [List
   commits](https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository)
 * [Compare two
   commits](https://developer.github.com/v3/repos/commits/#compare-two-commits)
 * [Contents](https://developer.github.com/v3/repos/contents/)
 * [Deployments](https://developer.github.com/v3/repos/deployments/)
 * [Merging](https://developer.github.com/v3/repos/merging/)
 * [Pages](https://developer.github.com/v3/repos/pages/)
 * [Get the latest
   release](https://developer.github.com/v3/repos/releases/#get-the-latest-release)
 * [Get a release by tag name](https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name)
 * [List assets for a
   release](https://developer.github.com/v3/repos/releases/#list-assets-for-a-release)
 * [Get a single release
   asset](https://developer.github.com/v3/repos/releases/#get-a-single-release-asset)
 * [Edit a release
   asset](https://developer.github.com/v3/repos/releases/#edit-a-release-asset)
 * [Delete a release
   asset](https://developer.github.com/v3/repos/releases/#delete-a-release-asset)
 * [Get the last year of commit activity
   data](https://developer.github.com/v3/repos/statistics/#get-the-last-year-of-commit-activity-data) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the number of additions and deletions per week](https://developer.github.com/v3/repos/statistics/#get-the-number-of-additions-and-deletions-per-week) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the weekly commit count for the repository owner and everyone else](https://developer.github.com/v3/repos/statistics/#get-the-weekly-commit-count-for-the-repository-owner-and-everyone-else) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Get the number of commits per hour in each day](https://developer.github.com/v3/repos/statistics/#get-the-number-of-commits-per-hour-in-each-day) (see [#86](https://github.com/mirage/ocaml-github/issues/86))
 * [Ping a
   hook](https://developer.github.com/v3/repos/hooks/#ping-a-hook)
 * [PubSubHubbub](https://developer.github.com/v3/repos/hooks/#pubsubhubbub)
 * [Receiving Webhooks
   helpers](https://developer.github.com/v3/repos/hooks/#receiving-webhooks)

### [Search](https://developer.github.com/v3/search/)
*Supported*:

 * [Search
   repositories](https://developer.github.com/v3/search/#search-repositories)

*Not yet supported*:

 * [Search code](https://developer.github.com/v3/search/#search-code)
 * [Search issues](https://developer.github.com/v3/search/#search-issues)
 * [Search users](https://developer.github.com/v3/search/#search-users)
 * [Text match media
    type](https://developer.github.com/v3/search/#text-match-metadata)

### [Users](https://developer.github.com/v3/users/)
*Supported*:

 * [Get a single
   user](https://developer.github.com/v3/users/#get-a-single-user)
 * [Get the authenticated
   user](https://developer.github.com/v3/users/#get-the-authenticated-user)

*Not yet supported*:

 * [Update the authenticated
   user](https://developer.github.com/v3/users/#update-the-authenticated-user)
 * [Get all users](https://developer.github.com/v3/users/#get-all-users)

### [Enterprise](https://developer.github.com/v3/enterprise/)
*Not yet supported*: everything
