open Lwt

module Store = Git_unix.Memory

let token = None
let owner = "mirage"
let repository = "ocaml-github"

let ( / ) = Filename.concat

module GitData = Github.GitData(Git.SHA)(Store.Digest)(Git.Blob)
