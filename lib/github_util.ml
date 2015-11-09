let validate_uri s =
  try let _ = Uri.of_string s in true
  with exn -> false
