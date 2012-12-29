let token = Config.access_token

let user =
  Lwt_main.run (
    Github.Monad.run (
      Github.User.current_info ~token ()
    )
  )

let _ =
  Printf.printf "current user: %s\n" (Github_j.string_of_user_info user)
