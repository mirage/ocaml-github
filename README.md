GitHub APIv3 OCaml Library
==========================

This library provides an OCaml interface to the Github JSON API.  It is not
yet complete, but look in `lib/github.atd` for the specification that has
been implemented so far.  The full API details can be found at:
<http://developer.github.com/v3/>

There are several tests in `lib_test` for small bits of functionality, and for
the authenticated ones you will need to edit `lib_test/config.ml` with suitable
auth information.  Use the `get_token` test to obtain an oAuth token for the
config file.

Two environment variables will cause more debugging to be output:

    GITHUB_DEBUG=1   # API calls output to stderr
    COHTTP_DEBUG=1   # even more HTTP-level debugging

If using the bindings from the toplevel, you can also set `Github.log_active`
to `true` to get the same effect as setting the `GITHUB_DEBUG` environment
variable.

Cookie jar
==========

Applications that use this library will need to save authentication tokens
locally, and the `Github_cookie_jar` module in the `unix/` directory helps
handle this more naturally.  It maps an authentication token onto a local
application name, which can then query that token at runtime and use it in
Github API calls.

The tokens are all stored in `$HOME/.github/<name>`, where `<name>` is the
local name of the application.

A `git-jar` command is also installed to add, remove and list the contents
of this cookie jar.

```console
$ git jar
```

...will display the man page.

```console
$ git jar show avsm
Enter Github password: **********                                                                                                                                                                                                                                               
Cookie Name | ID       | Application                              | Note      
----------------------------------------------------------------------------------
   <remote> | 236241   | Real World OCaml                         |           
   <remote> | 340988   | Travis                                   |           
```

Note that the cookies are all current remote.  To save the Real World OCaml one
to local storage so that an application can use it, just do:

```console
$ git jar save avsm 236241 rwo
Enter Github password: **********                                                                                                                                                                                                                                               
Github cookie jar: created /Users/avsm/.github/jar/rwo
```

This will now show up in the `show` command as a local cookie:

```console
$ git jar show avsm
Enter Github password: **********                                                                                                                                                                                                                                               
Cookie Name | ID       | Application                              | Note      
----------------------------------------------------------------------------------
        rwo | 236241   | Real World OCaml                         |           
   <remote> | 340988   | Travis                                   |           
```

Your Github application can how use it via the `Github_cookie_jar` module:

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

Manipulate GitHub releases
==========================

The [Releases](https://developer.github.com/v3/repos/releases/) API in
GitHub cannot itself be synched via Git, so this command-line tool lets
you specify a src user/repo and destination user/repo pair, and copies
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
