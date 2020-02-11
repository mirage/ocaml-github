let token = Config.access_token

let test_current_user =
  (* Test if the current user, associated with the access token, is correctly
     flagged as [`User]. *)
  let current_user =
    Lwt_main.run begin
      Github.(Monad.(run (User.current_info ~token () >|= Response.value)))
    end
  in
  assert (current_user.Github_t.user_info_ty = `User);
  Format.printf "Check current user: OK.@."

let test_current_user_first_org =
  (* Test if the current user, associated with the access token, first
     organization in the list is correctly flagged as [`Org]. *)
  let org =
    Lwt_main.run begin
      Github.(Monad.(run begin
          let orgs = Organization.current_user_orgs ~token () in
          Stream.next orgs >>= function
          | None ->
            Printf.eprintf "No organizations for the current user.\n";
            exit 1
          | Some (first_org, _) ->
            return first_org
        end))
    end
  in
  assert (org.Github_t.org_ty = `Org);
  Format.printf "Check current user first org: OK.@."

let test_ocaml_organization =
  (* Test if the OCaml organization (using the [/user/...] API) is correctly
     flagged as [`Org]. *)
  let ocaml_user =
    Lwt_main.run begin
      Github.(Monad.(run begin
          User.info ~token ~user:"ocaml" () >|= Response.value
        end))
    end
  in
  assert (ocaml_user.Github_t.user_info_ty = `Org);
  Format.printf "Check OCaml org: OK.@."

let test_ocaml_repository =
  (* Test if the owner of the [opam] repository (using the [/repos/...] APIS) is
     correctly flag as [`Org]. *)
  let opam_repository =
    Lwt_main.run begin
      Github.(Monad.(run begin
          Repo.info ~token ~user:"ocaml" ~repo:"opam" () >|= Response.value
        end))
    end
  in
  assert Github_t.(opam_repository.repository_owner.user_ty = `Org);
  Format.printf "Check OCaml repo: OK.@."
