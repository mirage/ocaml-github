This library provides an OCaml/Lwt interface to the Github JSON API.  It is not
yet complete, but look in `lib/github.atd` and `lib/github.ml` for example
code, and fill in the API details from http://developer.github.com/v3/

There are several tests in `lib_test`, and for the authenticated ones you will
need to edit `lib_test/config.ml` with suitable auth information.  Use the
`get_token` test to obtain an oAuth token for the config file.

