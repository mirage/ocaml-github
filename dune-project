(lang dune 2.7)
(name github)

(generate_opam_files true)
(formatting disabled)

(license MIT)
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(authors "Anil Madhavapeddy" "David Sheets" "Andy Ray"
  "Jeff Hammerbacher" "Thomas Gazagnaire" "Rudi Grinberg"
  "Qi Li" "Jeremy Yallop" "Dave Tucker")
(source (github mirage/ocaml-github))
(documentation "https://mirage.github.io/ocaml-github/")

(package
 (name github)
 (tags (org:mirage org:xapi-project git))
 (depends
  (ocaml (>= 4.08.0))
  (uri (>= 1.9.0))
  (cohttp (>= 4.0.0))
  (lwt (>= 2.4.4))
  (cohttp-lwt (>= 4.0.0))
  (github-data (= :version))
  (yojson (>= 1.7.0))
  stringext)
 (synopsis "GitHub APIv3 OCaml library")
 (description "This library provides an OCaml interface to the
[GitHub APIv3](https://docs.github.com/rest/) (JSON).

It is compatible with [MirageOS](https://mirage.io) and also compiles to pure
JavaScript via [js_of_ocaml](http://ocsigen.org/js_of_ocaml)."))

(package
 (name github-jsoo)
 (tags (org:mirage org:xapi-project git))
 (depends
  (ocaml (>= 4.08.0))
  (github (= :version))
  (cohttp (>= 4.0.0))
  (cohttp-lwt-jsoo (>= 4.0.0))
  (js_of_ocaml-lwt (>= 3.4.0)))
 (synopsis "GitHub APIv3 JavaScript library")
 (description "This library provides an OCaml interface to the [GitHub APIv3](https://docs.github.com/rest/)
(JSON). This library installs the JavaScript version, which uses [js_of_ocaml](http://ocsigen.org/js_of_ocaml)."))

(package
 (name github-unix)
 (tags (org:mirage org:xapi-project git))
 (depends
  (ocaml (>= 4.08.0))
  (github (= :version))
  (cohttp (>= 4.0.0))
  (cohttp-lwt-unix (>= 4.0.0))
  stringext
  (cmdliner (< 1.1.0))
  base-unix
  lwt)
 (synopsis "GitHub APIv3 Unix library")
 (description "This library provides an OCaml interface to the [GitHub APIv3](https://docs.github.com/rest/)
(JSON).  This package installs the Unix (Lwt) version."))

(package
 (name github-data)
 (tags (org:mirage org:xapi-project git))
 (depends
   (ocaml (>= 4.08.0))
   (yojson (>= 1.7.0))
   (atdgen (>= 2.0.0)))
 (conflicts (github (<> :version)))
 (synopsis "GitHub APIv3 data library")
 (description "This library provides an OCaml interface to the [GitHub APIv3](https://docs.github.com/rest/)
(JSON).  This package installs the data conversion library."))
