let token = Config.access_token

let user =
  Lwt_main.run (
    Github.(Monad.(run (
      User.current_info ~token () >|= Response.value
    )))
  )

let _ =
  Printf.printf "current user: %s\n" (Github_j.string_of_user_info user)
