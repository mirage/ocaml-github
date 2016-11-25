#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let unix = Conf.with_pkg "unix"
let js = Conf.with_pkg "js"

let () =
  Pkg.describe "github" @@ fun c ->
  let unix = Conf.value c unix in
  let js = Conf.value c js in
  (* let example x = Pkg.test ~cond:unix ~run:false ("examples/" ^ x) in *)
  Ok [
    Pkg.lib ~exts:Exts.interface  "lib/github_s";
    Pkg.mllib "lib/github_core.mllib";
    Pkg.mllib ~cond:unix "unix/github_unix.mllib";
    Pkg.mllib ~cond:js "js/github_js.mllib";
    (* examples + binaries *)
  ]
