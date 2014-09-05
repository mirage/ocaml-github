module Github' = Github_core.Make(Cohttp_lwt_unix)

include Github'

