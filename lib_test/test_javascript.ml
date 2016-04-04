(* js utils *)

let get_element e =
  let d = Dom_html.document in
  Js.Opt.get
    (d##getElementById (Js.string e))
    (fun () -> assert false)

let get_input n =
  match Dom_html.tagged (get_element n) with
  | Dom_html.Input(x) -> x
  | _ -> failwith ("couldn't find text element" ^ n)

let value n = Js.to_string (get_input n)##.value

(* grab elements from the webpage *)

let gist_fetch = get_element "gist-fetch"
let gist_result = get_element "gist-result"
let repos_list = get_element "repos-list"
let repos_result = get_element "repos-result"

(* github api demo *)

open Lwt.Infix
open Github_js

let run _ =

  let open Printf in
  let open Github_t in

  let run_gist_fetch _ =
    Lwt.ignore_result
      (Monad.(run (
         Gist.get ~id:(value "gist-id") () >|= Response.value
       )) >>= fun gist ->
       let content =
         try (* show content of 1st file in gist *)
           match (snd (List.nth gist.gist_files 0)).gist_file_content with
           | Some(c) -> c
           | _ -> raise Not_found
         with _ ->
           "NO CONTENT"
       in
       gist_result##.innerHTML := Js.string content;
       Lwt.return ());
    Js._false
  in
  let run_repos_list _ =
    Lwt.ignore_result
      (Monad.run (Stream.to_list (User.repositories ~user:(value "username") ())) >>= fun repos ->
       let buf = Buffer.create 1024 in
       Buffer.add_string buf "<table>";
       List.iter (fun repo ->
         Buffer.add_string buf "<tr><td>";
         Buffer.add_string buf repo.repository_full_name;
         Buffer.add_string buf "</td></tr> ") repos;
       Buffer.add_string buf "</table>";
       repos_result##.innerHTML := Js.string (Buffer.contents buf);
       Lwt.return ());
    Js._false
  in

  gist_fetch##.onclick := Dom_html.handler run_gist_fetch;
  repos_list##.onclick := Dom_html.handler run_repos_list;

  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler run

