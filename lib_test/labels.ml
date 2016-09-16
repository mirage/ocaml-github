open Lwt
open Printf

let token = Config.access_token
let user = "ocaml"
let repo = "opam"

let t =
  let open Github in
  let open Monad in
  let open Github_t in
  run (
    let labels = Label.for_repo ~token ~user ~repo () in
    Stream.iter (fun label ->
      let name = label.label_name in
      eprintf "label %s" name;
      return ()
    ) labels
  )

;;
Lwt_main.run t
