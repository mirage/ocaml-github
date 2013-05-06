(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

let rec mkdir_p dir =
  match Sys.file_exists dir with
    |true -> ()
    |false ->
        mkdir_p (Filename.dirname dir);
        Unix.mkdir dir 0o700

let init () =
  match Sys.file_exists jar with
  |true -> ()
  |false ->
    printf "Github cookie jar: initialized %s\n" jar;
    mkdir_p jar

(* Save an authentication token to disk, under the [name]
 * file in the jar *)
let save ~name ~auth =
  if List.exists (fun re -> Re.execp re name) invalid_names then
    raise (InvalidName name);
  let open Unix in
  init ();
  (* Backup any old one *)
  let tm = gmtime (gettimeofday ()) in
  let backfname = sprintf "%s.%.4d%.2d%.2d.%2d%2d%2d.bak"
    name tm.tm_year tm.tm_mon tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec in
  let fullname = Filename.concat jar name in
  let fullback = Filename.concat jar backfname in
  if Sys.file_exists fullname then begin
    printf "Github cookie jar: backing up\n%s -> %s\n" fullname fullback;
    Unix.rename fullname fullback;
  end;
  mkdir_p (Filename.dirname fullname);
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
        lwt stats = Lwt_unix.stat path in Lwt_unix.(
        match stats.st_kind with
          | S_REG ->
              lwt auth = read_auth_file ident in
              return ((ident,auth)::a)
          | S_DIR ->
              lwt sub = traverse ident in
              return (sub@a)
          | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK -> return a
        )
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
