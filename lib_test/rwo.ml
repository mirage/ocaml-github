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

open Lwt
open Printf
open Cohttp
open Cohttp_lwt_unix
open Config

let scopes = [`Public_repo] 
let step1_link = Github.URI.authorize ~scopes ~client_id ()
  
module Resp = struct
  let wrap_html s =
    sprintf "<html><body>%s</body></html>" s

  (* respond with an error *)
  let not_found req err = 
    let status = `Not_found in
    let headers = Header.of_list [ "Cache-control", "no-cache" ] in
    let body = sprintf "<html><body><h1>Error</h1><p>%s</p></body></html>" err in
    Server.respond_string ~headers ~status ~body ()

  (* internal error *)
  let internal_error err = 
    let status = `Internal_server_error in
    let headers = Header.of_list [ "Cache-control", "no-cache" ] in
    let body = sprintf "<html><body><h1>Internal Server Error</h1><p>%s</p></body></html>" err in
    Server.respond_string ~headers ~status ~body

  (* dynamic response *)
  let dyn req body =
    let status = `OK in
    Server.respond_string ~body ~status ()

  (* index page *)
  let index req =
    let body = wrap_html (sprintf "<a href=\"%s\">step1</a>" (Uri.to_string step1_link)) in
    dyn req body

  (* dispatch non-file URLs *)
  let dispatch req =
    function
    | ["";""]
    | ["";"index.html"]  ->
        index req
    | ["";"step2"] -> begin
        let uri = Request.uri req in
        let code = match Uri.get_query_param uri "code" with
          |Some hd -> hd |None -> "" in
        try_lwt begin
          match_lwt Github.Token.of_code ~client_id ~client_secret ~code () with
          |None -> internal_error "no token in response" ()
          |Some token -> dyn req (wrap_html ("ok: token is "^(Github.Token.to_string token)))
        end with Failure e ->
          dyn req (wrap_html ("err: "^e))
    end
    | _ -> 
        not_found req "dispatch"
end

(* main callback function *)
let callback con_id req body =
  let uri = Request.uri req in
  let path = Uri.path uri in
  printf "%s %s [%s]\n%!" (Code.string_of_method (Request.meth req)) path 
    (String.concat "," (List.map (fun (h,v) -> sprintf "%s=%s" h (String.concat "," v)) (Uri.query uri)));
  (* normalize path to strip out ../. and such *)
  let path_elem = Stringext.(split ~on:'/' (Uri.path uri)) in
  List.iter (fun p -> printf "> %s\n%!" p) path_elem;
  Resp.dispatch req path_elem 

let server_t =
  let port = 8080 in
  let conn_closed con_id = () in
  let spec = Cohttp_lwt_unix.Server.make ~callback ~conn_closed () in
  let ctx = Cohttp_lwt_unix_net.init () in
  let mode = `TCP (`Port port) in
  Cohttp_lwt_unix.Server.create ~ctx ~mode spec

let _ =
  Lwt_main.run server_t
