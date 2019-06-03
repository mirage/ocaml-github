open Github_t

let token = Config.access_token

let t = Github.(Monad.(run (
    let orgs = Organization.current_user_orgs ~token () in
    Stream.next orgs
    >>= function
    | None -> Printf.eprintf "no orgs for current user\n"; exit 1
    | Some (first_org, _) ->
      Printf.eprintf "org %Ld: %s\n%!" first_org.org_id first_org.org_login;
      return ()
)))

;;

Lwt_main.run t
