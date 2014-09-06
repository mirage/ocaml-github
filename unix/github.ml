module Github' = Github_core.Make(Cohttp_lwt_unix.Client)

include Github'

