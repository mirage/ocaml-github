open Lwt

module Store = Git_unix.FS

let token = None
let owner = "mirage"
let repo  = "ocaml-github"

let sp = Printf.sprintf

let red     fmt = sp ("\027[31m" ^^ fmt ^^ "\027[m")
let green   fmt = sp ("\027[32m" ^^ fmt ^^ "\027[m")
let yellow  fmt = sp ("\027[33m" ^^ fmt ^^ "\027[m")
let blue    fmt = sp ("\027[36m" ^^ fmt ^^ "\027[m")
let gray    fmt = sp ("\027[37m" ^^ fmt ^^ "\027[m")
let red_s       = red "%s"
let green_s     = green "%s"
let yellow_s    = yellow "%s"
let blue_s      = blue "%s"
let gray_s      = gray "%s"

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic); r
  with exn ->
    ignore (Unix.close_process_in ic); raise exn

let columns =
  try
    with_process_in "tput cols" (fun ic -> int_of_string (input_line ic))
  with _ -> try
    with_process_in "stty size" (fun ic ->
      match Stringext.split (input_line ic) ~on:' ' with
      | [_; v] -> int_of_string v
      | _ -> failwith "stty")
  with _ -> try int_of_string (Sys.getenv "COLUMNS")
  with _ -> 80

let line oc ?color c =
  let line = match color with
    | Some `Blue   -> blue_s (String.make columns c)
    | Some `Yellow -> yellow_s (String.make columns c)
    | None         -> String.make columns c
  in Printf.fprintf oc "%s\n%!" line

let left s n =
  let n = n - String.length s in
  if n <= 0 then s
  else s ^ (String.make n ' ')

let left_columns = 20

let print s = Printf.printf "%s%!" s
let newline () = Printf.printf "\n%!"

let t =
  Store.create ~root:(Sys.getenv "PWD") () >>= fun store ->
  Store.read_index store >>= fun head ->
  Lwt_list.iter_s
    (fun entrie ->
      Github.(Monad.(run (
        let module SHA_IO = Git.SHA.IO(Store.Digest) in
        let module GitData = Github.GitData
          (SHA_IO.Blob)(Git.Blob)
          (SHA_IO.Tree)(Git.Tree)
          (SHA_IO.Commit)(Git.Commit)
        in
        GitData.Blob.get ?token ~owner ~repo ~sha:entrie.Git.Index.id >|=
        Github.Response.value >|=
        GitData.Blob.make >>= fun blob ->
        Monad.embed (Store.read store (Git.SHA.of_blob entrie.Git.Index.id)) >>=
          (function
            | Some (Git.Value.Blob blob') ->

              let content = Git.Blob.to_raw blob' in
              let a = Git.SHA.Blob.equal blob.GitData.Blob.sha entrie.Git.Index.id in
              let b = blob.GitData.Blob.content = content in

              if a && b
              then print (left (green_s "[OK]") left_columns)
              else if a || b
              then print (left (yellow_s "[PARTIAL]") left_columns)
              else print (left (red_s "[ERROR]") left_columns);

              print (sp "%s %s"
                (blue_s (blob.GitData.Blob.sha |> Git.SHA.Blob.to_hex))
                entrie.Git.Index.name);
              newline ();

              return ()
            | _ -> return ())
    ))))
    head.Git.Index.entries

let () = Lwt_main.run t
