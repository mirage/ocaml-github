(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Printf
open Lwt

exception InvalidName of string
let invalid_names = Re.(List.map compile [
  seq [bos; str "."; eos];
  seq [bos; str ".."; eos];
  seq [bos; str Filename.dir_sep];
  seq [str Filename.dir_sep; eos];
])

(* Store Github auth tokens *)
let home = try Sys.getenv "HOME" with Not_found -> "."
let basedir = Filename.concat home ".github"
let jar = Filename.concat basedir "jar"

let file_kind_match path ~reg ~dir ~other = Lwt_unix.(
  lwt stats = stat path in
  match stats.st_kind with
    | S_REG -> reg ()
    | S_DIR -> dir ()
    | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> other ()
)

let rec mkdir_p dir =
  match Sys.file_exists dir with
    | true -> return ()
    | false ->
        mkdir_p (Filename.dirname dir);
        Lwt_unix.mkdir dir 0o700

let init () =
  match Sys.file_exists jar with
    | true -> return ()
    | false ->
        printf "Github cookie jar: initialized %s\n" jar;
        mkdir_p jar

(* Save an authentication token to disk, under the [name]
 * file in the jar *)
let save ~name ~auth =
  lwt () = if List.exists (fun re -> Re.execp re name) invalid_names then
    fail (InvalidName name)
  else
    return () in
  lwt () = init () in
  let rec backup_path ?(dirok=false) name =
    let fullname = Filename.concat jar name in
    let backup () =
      let open Unix in
      let tm = gmtime (gettimeofday ()) in
      let backfname = sprintf "%s.%.4d%.2d%.2d.%2d%2d%2d.bak"
        name (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
        tm.tm_hour tm.tm_min tm.tm_sec in
      let fullback = Filename.concat jar backfname in
      printf "Github cookie jar: backing up\n%s -> %s\n" fullname fullback;
      Lwt_unix.rename fullname fullback
    in
    try_lwt file_kind_match fullname
      ~reg:backup
      ~dir:(if dirok then return else backup)
      ~other:backup
    with
      | Unix.Unix_error (Unix.ENOENT, _, _)
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> begin
      match Filename.dirname name with
        | "." -> return ()
        | parent -> backup_path ~dirok:true parent
    end
  in
  lwt () = backup_path name in
  let fullname = Filename.concat jar name in
  lwt () = mkdir_p (Filename.dirname fullname) in
  let fout = open_out fullname in
  fprintf fout "%s" (Github_j.string_of_auth auth);
  close_out fout;
  printf "Github cookie jar: created %s\n" fullname;
  return ()

(* Read a JSON auth file in and parse it *)
let read_auth_file name =
  let fname = Filename.concat jar name in
  Lwt_io.with_file ~mode:Lwt_io.input fname
    (fun ic ->
      lwt buf = Lwt_stream.fold_s (fun b a -> return (a^b)) (Lwt_io.read_lines ic) "" in
      return (Github_j.auth_of_string buf)
    )

(* Retrieve all the cookies *)
let get_all () =
  init ();
  let rec traverse dir =
    let base = Filename.concat jar dir in
    let files = Lwt_unix.files_of_directory base in
    Lwt_stream.fold_s (fun b a ->
      if b = "." || b = ".." then return a else begin
        let path = Filename.concat base b in
        let ident = Filename.concat dir b in
        file_kind_match path
          ~reg:(fun () ->
            lwt auth = read_auth_file ident in
            return ((ident,auth)::a))
          ~dir:(fun () ->
            lwt sub = traverse ident in
            return (sub@a))
          ~other:(fun () -> return a)
    end
    ) files []
  in traverse ""

(* Get one cookie by name *)
let get ~name =
  init ();
  try_lwt
    lwt auth = read_auth_file name in
    return (Some auth)
  with _ -> return None
